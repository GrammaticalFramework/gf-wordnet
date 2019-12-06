{-# LANGUAGE CPP, MonadComprehensions #-}

import Database.Daison
import SenseSchema
import Text.JSON
import Text.JSON.String
import URLEncoding
import Network.CGI
import Network.FastCGI(runFastCGI,runFastCGIConcurrent')
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import System.IO hiding (ReadWriteMode)
import System.Environment
import System.Directory
import System.Posix.Files
import System.Process
import System.Exit
import Control.Monad(liftM2,liftM3,forM_)
import qualified Codec.Binary.UTF8.String as UTF8 (encodeString,decodeString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8 (toString,fromString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.UTF8 as BSS
import qualified Data.Map as Map
import Data.Char
import Data.Maybe

main = do
  db <- openDB (SERVER_PATH++"/semantics.db")
-- #ifndef mingw32_HOST_OS
--    runFastCGIConcurrent' forkIO 100 (cgiMain db)
-- #else
  runFastCGI (handleErrors $ cgiMain db)
-- #endif


cgiMain :: Database -> CGI CGIResult
cgiMain db = do
  mb_s1 <- getInput "code"
  mb_s2 <- getInput "user"
  mb_s3 <- getInput "update_id"
  mb_s4 <- getInput "get_id"
  mb_s5 <- getInput "lang"
  mb_s6 <- fmap (fmap (urlDecodeUnicode . UTF8.decodeString)) $ getInput "def"
  mb_s7 <- getInput "commit"
  mb_s8 <- getInput "push"
  mb_s9 <- getInput "author"
  case mb_s1 of
    Just code -> doLogin code
    Nothing   -> case liftM3 doGet mb_s2 mb_s4 mb_s5 of
                   Just action -> do res <- liftIO action
                                     case res of
                                       Just def -> outputJSONP (showJSON def)
                                       Nothing  -> outputNothing
                   Nothing     -> case liftM3 doUpdate mb_s2 mb_s3 mb_s5 of
                                    Just action -> do json <- liftIO (action mb_s6)
                                                      outputJSONP json
                                    Nothing     -> case liftM3 doCommit mb_s2 mb_s9 mb_s7 of
                                                     Just action -> do res <- liftIO action
                                                                       outputJSONP res
                                                     Nothing     -> case fmap doPull mb_s8 of
                                                                      Just action -> do s <- liftIO action
                                                                                        outputText "text/plain; charset=utf-8" s
                                                                      Nothing     -> outputNothing
  where
    doLogin code = do
      man <- liftIO $ newManager tlsManagerSettings
      res <- liftIO $ do
               client_secret <- getEnv "GF_WORDNET_CLIENT_SECRET"
               req <- parseRequest ("https://github.com/login/oauth/access_token?client_id=1e94c97e812a9f502068&client_secret="++client_secret++"&code="++code)
               httpLbs req man
      case lookup "access_token" (formDecode (UTF8.toString (responseBody res))) of
        Just token -> do res <- liftIO $ do
                                  req0 <- parseRequest ("https://api.github.com/user?access_token="++token)
                                  let req = req0{requestHeaders=(hUserAgent,BSS.fromString "GF WordNet"):requestHeaders req0}
                                  httpLbs req man
                         case (do JSObject obj <- runGetJSON readJSObject (UTF8.toString (responseBody res))
                                  user  <- resultToEither (valFromObj "login" obj)
                                  name  <- resultToEither (valFromObj "name" obj)
                                  email <- resultToEither (valFromObj "email" obj)
                                  return (user, name++" <"++email++">")) of
                           Right (user,author) -> do count <- liftIO (runDaison db ReadOnlyMode $
                                                                        fmap length $ select (from updates_usr everything))
                                                     setCookie ((newCookie "user" user){cookiePath=Just "/wordnet"})
                                                     setCookie ((newCookie "author" author){cookiePath=Just "/wordnet"})
                                                     setCookie ((newCookie "count" (show count)){cookiePath=Just "/wordnet"})
                                                     redirect "/wordnet"
                           Left err     -> output err
        Nothing    -> outputNothing

    doGet user lex_id lang = do
      res <- runDaison db ReadWriteMode $ do
               select (fromIndex updates_idx (at (user,lex_id,lang)))
      case res of
        (_,u):_ -> do return (Just (def u))
        _       -> do s <- readSourceFile lang
                      return (getDefinition lex_id s)

    doUpdate user lex_id lang mb_def = do
      mb_def' <- doGet user lex_id lang
      let (def,s) = case mb_def of
                      Just def | mb_def /= mb_def' -> (def,                  Changed)
                      _                            -> (fromMaybe "" mb_def', Checked)
      runDaison db ReadWriteMode $ do
        res <- update lexemes [(id, lex{status=updateStatus lang s (status lex)}) | (id,lex) <- fromIndex lexemes_fun (at lex_id)]
        insert_ updates (Update user lex_id lang def)
        count <- fmap length $ select (from updates_usr everything)
        return (count,head [map toLower (show st)
                              | (_,lexeme) <- res,
                                (lang',st) <- status lexeme,
                                lang==lang'])
      where
        updateStatus lang s []                  = [(lang,s)]
        updateStatus lang s ((lang',s'):status)
          | lang==lang'                         = (lang,s) : status
        updateStatus lang s (x:status)          = x : updateStatus lang s status

    doCommit user author commit = do
      res <- runDaison db ReadWriteMode $ do
               res <- select [(lang,[(lex_id,def)]) | (_,Update _ lex_id lang def) <- fromIndex updates_usr (at user)]
               delete updates (from updates_usr (at user))
               return res
      forM_ ((Map.toList . Map.fromListWith (++)) res) $ \(lang,ids) ->
        if null ids
          then return ()
          else do let fname = "WordNet"++drop 5 lang++".gf"
                  ls <- fmap lines $ readSourceFile lang
                  (tmp_fname,hTmp) <- openTempFile SERVER_PATH fname
                  hSetEncoding hTmp utf8
                  mapM_ (hPutStrLn hTmp . annotate ids) ls 
                  hClose hTmp
                  setFileMode tmp_fname (ownerReadMode `unionFileModes`
                                         ownerWriteMode `unionFileModes`
                                         groupReadMode `unionFileModes`
                                         groupWriteMode `unionFileModes`
                                         otherReadMode)
                  renameFile tmp_fname (SERVER_PATH++"/"++fname)
      (res,out,err) <- git ["commit","--author",author,"--message","progress","WordNet*.gf"]
      return (unwords ["$", "git", "commit","--author",author,"--message","progress","WordNet*.gf"]++"\n"++out++"\n"++err++"\n"++show res)
      where
        annotate updated l =
          case words l of
            ("lin":id:"=":_) -> case lookup id updated of
                                  Just def -> "lin "++id++" = "++def++" ;"
                                  _        -> l
            _                              -> l

    doPull _ = do
      (res,out,err) <- git ["pull","--no-edit"]
      return (out++"\n"++err++"\n"++show res)

git args = readCreateProcessWithExitCode (proc "git" args){cwd=Just SERVER_PATH} ""

readSourceFile lang = do
  let fname = "WordNet"++drop 5 lang++".gf"
  hInp <- openFile (SERVER_PATH++"/"++fname) ReadMode
  hSetEncoding hInp utf8
  hGetContents hInp                

getDefinition w s = seek s
  where
    seek []                        = Nothing
    seek ('\n':'l':'i':'n':' ':cs) = match w (dropWhile isSpace cs)
    seek (c:cs)                    = seek cs

    match []     cs     = case dropWhile isSpace cs of
                            ('=':cs) -> Just (def 0 (dropWhile isSpace cs))
                            cs       -> seek cs
    match (d:ds) (c:cs)
      | d == c          = match ds cs
      | otherwise       = seek cs

    def c []        = []
    def 0 (c@';' :cs) = []
    def i (c@'(' :cs) = c:def (i+1)         cs
    def i (c@'[' :cs) = c:def (i+1)         cs
    def i (c@'{' :cs) = c:def (i+1)         cs
    def i (c@')' :cs) = c:def (max 0 (i-1)) cs
    def i (c@']' :cs) = c:def (max 0 (i-1)) cs
    def i (c@'}' :cs) = c:def (max 0 (i-1)) cs
    def i (c@'"' :cs) = c:str i c cs
    def i (c@'\'':cs) = c:str i c cs
    def i (c@' ' :cs) = case def i cs of
                          [] -> []
                          cs -> c:cs
    def i (c     :cs) = c:def i cs

    str i d []          = []
    str i d ('\\':c:cs) = '\\':c:str i d cs
    str i d (     c:cs)
      | d == c             = c:def i   cs
      | otherwise          = c:str i d cs

outputJSONP :: JSON a => a -> CGI CGIResult
outputJSONP = outputEncodedJSONP . encode

outputEncodedJSONP :: String -> CGI CGIResult
outputEncodedJSONP json = 
    do mc <- getInput "jsonp"
       let (ty,str) = case mc of
                        Nothing -> ("json",json)
                        Just c  -> ("javascript",c ++ "(" ++ json ++ ")")
           ct = "application/"++ty++"; charset=utf-8"
       outputText ct str

outputText ct = outputStrict ct . UTF8.encodeString

outputStrict :: String -> String -> CGI CGIResult
outputStrict ct x = do setHeader "Content-Type" ct
                       setHeader "Content-Length" (show (length x))
                       setXO
                       output x

setXO = setHeader "Access-Control-Allow-Origin" "*"

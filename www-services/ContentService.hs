{-# LANGUAGE CPP, MonadComprehensions, BangPatterns #-}

import Database.Daison
import SenseSchema
import ContentSchema
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
import System.Random
import System.Process
import System.Exit
import Control.Monad(liftM2,liftM3,liftM4,forM_,forM)
import Control.Concurrent
import qualified Codec.Binary.UTF8.String as UTF8 (encodeString,decodeString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8 (toString,fromString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.UTF8 as BSS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Data.Maybe
import PGF2

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
  mb_s10<- getInput "token"
  mb_s11<- getInput "pick_example"
  mb_s12<- getInput "update_example"
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
                                    Nothing     -> case liftM4 doCommit mb_s2 mb_s9 mb_s10 mb_s7 of
                                                     Just action -> do setHeader "Cache-Control" "no-cache"
                                                                       setHeader "X-Content-Type-Options" "nosniff"
                                                                       setHeader "Content-Type" "text/plain; charset=utf-8"
                                                                       res <- liftIO action
                                                                       outputFPS res
                                                     Nothing     -> case fmap doPull mb_s8 of
                                                                      Just action -> do setHeader "Content-Type" "text/plain; charset=utf-8"
                                                                                        s <- liftIO action
                                                                                        outputFPS s
                                                                      Nothing     -> case fmap doPickExample mb_s11 of
                                                                                        Just action -> liftIO action >>= outputJSONP
                                                                                        Nothing     -> case liftM3 doUpdateExample mb_s2 (fmap read mb_s12) mb_s6 of
                                                                                                         Just action -> liftIO action >>= outputJSONP
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
                                  req0 <- parseRequest ("https://api.github.com/user")
                                  let req = req0{requestHeaders=(hUserAgent,BSS.fromString "GF WordNet"):
                                                                (hAuthorization, BSS.fromString ("token "++token)):
                                                                requestHeaders req0}
                                  httpLbs req man
                         case (do res <- runGetJSON readJSObject (UTF8.toString (responseBody res))
                                  obj <- case res of
                                           JSObject obj -> return obj
                                           _            -> fail "Didn't get an object from api.github.com"
                                  user  <- resultToEither (valFromObj "login" obj)
                                  name  <- resultToEither (valFromObj "name" obj)
                                  email <- resultToEither (valFromObj "email" obj)
                                  return (user, name++" <"++email++">")) of
                           Right (user,author) -> do count <- liftIO (runDaison db ReadOnlyMode $
                                                                        fmap length $ select (from updates_usr everything))
                                                     setCookie ((newCookie "user" user){cookiePath=Just "/wordnet"})
                                                     setCookie ((newCookie "author" author){cookiePath=Just "/wordnet"})
                                                     setCookie ((newCookie "token" token){cookiePath=Just "/wordnet"})
                                                     setCookie ((newCookie "count" (show count)){cookiePath=Just "/wordnet"})
                                                     redirect "/wordnet"
                           Left err     -> output err
        Nothing    -> outputNothing

    doGet user lex_id lang = do
      res <- runDaison db ReadWriteMode $ do
               select (fromIndex updates_lex_idx (at (user,lex_id,lang)))
      case res of
        (_,u):_ -> return (Just (def u))
        _       -> fmap (Map.lookup lex_id) (getDefinitions (Set.singleton lex_id) lang)

    doUpdate user lex_id lang mb_def = do
      mb_def' <- doGet user lex_id lang
      let (def,s) = case mb_def of
                      Just def | mb_def /= mb_def' -> (def,                  Changed)
                      _                            -> (fromMaybe "" mb_def', Checked)
      runDaison db ReadWriteMode $ do
        res <- update lexemes [(id, lex{status=updateStatus lang s (status lex)}) | (id,lex) <- fromIndex lexemes_fun (at lex_id)]
        insert_ updates (UpdateLexeme user lex_id lang def)
        c <- query countRows (from updates_usr everything)
        return (c,head [map toLower (show st)
                              | (_,lexeme) <- res,
                                (lang',st) <- status lexeme,
                                lang==lang'])
      where
        updateStatus lang s []                  = [(lang,s)]
        updateStatus lang s ((lang',s'):status)
          | lang==lang'                         = (lang,s) : status
        updateStatus lang s (x:status)          = x : updateStatus lang s status

    doCommit user author token commit = do
      (out,inp) <- createPipe
      forkIO (patchUp inp >>
              hPutStrLn inp "" >>
              hFlush inp >>
              git inp [["commit","--author",author,"--message","progress","WordNet*.gf","examples.txt"]
                      ,["push", "https://"++user++":"++token++"@github.com/GrammaticalFramework/gf-wordnet"]
                      ])
      BS.hGetContents out
      where
        patchUp inp = do
          res <- runDaison db ReadWriteMode $ do
                   res <- query groupRows [(fileName u,u) | (_,u) <- fromIndex updates_usr (at user)]
                   delete updates (from updates_usr (at user))
                   return res
          forM_ (Map.toList res) $ \(fname,us) ->
            if null us
              then return ()
              else do hPutStrLn inp ("Patch up "++fname)
                      hFlush inp
                      ls <- fmap lines $ readUtf8File fname
                      (tmp_fname,hTmp) <- openTempFile SERVER_PATH fname
                      hSetEncoding hTmp utf8
                      mapM_ (hPutStrLn hTmp) (annotate fname us 1 ls)
                      hClose hTmp
                      setFileMode tmp_fname (ownerReadMode `unionFileModes`
                                             ownerWriteMode `unionFileModes`
                                             groupReadMode `unionFileModes`
                                             groupWriteMode `unionFileModes`
                                             otherReadMode)
                      renameFile tmp_fname (SERVER_PATH++"/"++fname)

        fileName (UpdateLexeme _ lex_id lang def) = "WordNet"++drop 5 lang++".gf"
        fileName (UpdateExample _ _ _)            = "examples.txt"

        annotate fname updates !line_no []     = []
        annotate fname updates !line_no (l:ls)
          | fname == "examples.txt"            =
              case [def | UpdateExample _ no def <- updates, line_no==no] of
                (def:_) -> let def_ls = lines def
                               n      = length def_ls
                           in def_ls ++ annotate fname updates (line_no+n) (drop n (l:ls))
                _       -> l : annotate fname updates (line_no+1) ls
        annotate fname updates !line_no (l:ls) =
          (case words l of
             ("lin":id:"=":_) -> case [def | UpdateLexeme _ lex_id lang def <- updates, lex_id==id] of
                                   (def:_) -> "lin "++id++" = "++def++" ;"
                                   _       -> l
             _                -> l)  : annotate fname updates (line_no+1) ls
             

    doPull _ = do
      (out,inp) <- createPipe
      git inp [["pull","--no-edit"]]
      BS.hGetContents out

    doPickExample _ = do
      g  <- newStdGen
      ls <- fmap lines $ readUtf8File "examples.txt"
      return (showJSON (pick g 36000 1 ls))
      where
        pick :: StdGen -> Int -> Int -> [String] -> (Int,String)
        pick g n line_no ls = find i line_no ls
          where
            (i,g') = randomR (0,n) g

            find i !line_no []     = pick g' i 1 ls
            find i !line_no (l:ls)
              | take 8 l == "abs* Phr"
                                   = if i == 0
                                       then (line_no,unlines (l:take 4 ls))
                                       else find (i-1) (line_no+1) ls
              | otherwise          = find i (line_no+1) ls

    doUpdateExample user line_no def =
      case map (readExpr . drop 4) def_ls of
        (Just e : _) -> do let fns = exprFunctions e
                           lex_defs <- fmap Map.fromList (mapM (\lang -> fmap ((,) lang)
                                                                              (getDefinitions (Set.fromList fns) lang))
                                                               ["ParseBul","ParseSwe"])
                           runDaison db ReadWriteMode $ do
                             insert_ updates (UpdateExample user line_no def)
                             ex_id <- insert_ examples (e,[])
                             st <- fmap concat (mapM (updateLexeme ex_id lex_defs) fns)
                             c  <- query countRows (from updates_usr everything)
                             return (showJSON (c,st))
        _            -> fail "Invalid expression"
      where
        def_ls = lines def

        updateLexeme ex_id lex_defs lex_id = do
          res <- select (fromIndex lexemes_fun (at lex_id))
          forM res $ \(id,lex) -> do
            status' <- mapM check (status lex)
            store lexemes (Just id)
                          (lex{status     =status',
                               example_ids=if elem lex_id (words (last def_ls)) && not (elem ex_id (example_ids lex))
                                             then ex_id:example_ids lex
                                             else example_ids lex
                              })
            return (lex_id,[(lang,map toLower (show st)) | (lang,st) <- status', Map.member lang lex_defs])
          where
            check (lang,st)
              | st == Unchecked || st == Guessed
                          = case Map.lookup lang lex_defs >>= Map.lookup lex_id of
                              Just def -> do insert_ updates (UpdateLexeme user lex_id lang def)
                                             return (lang,Checked)
                              Nothing  -> do return (lang,st)
              | otherwise = return (lang,st)

git inp []                 = hClose inp
git inp (command:commands) = do
  hPutStrLn inp (unwords ("$":"git":map censor command))
  hFlush inp
  (_,_,_,ph) <- createProcess_ "git"
                               (proc "git" command){std_out=UseHandle inp
                                                   ,std_err=UseHandle inp
                                                   ,cwd=Just SERVER_PATH
                                                   }
  code <- waitForProcess ph
  case code of
    ExitSuccess -> git inp commands
    _           -> hClose inp
  where
    censor ('h':'t':'t':'p':'s':':':'/':'/':cs) = 
      'h':'t':'t':'p':'s':':':'/':'/':drop 1 (dropWhile (/='@') cs)
    censor s = s

readUtf8File fname = do
  hInp <- openFile (SERVER_PATH++"/"++fname) ReadMode
  hSetEncoding hInp utf8
  hGetContents hInp

getDefinitions lex_ids lang = do
  s <- readUtf8File ("WordNet"++drop 5 lang++".gf")
  return (seek lex_ids s)
  where
    seek lex_ids _  | Set.null lex_ids     = Map.empty
    seek lex_ids []                        = Map.empty
    seek lex_ids ('\n':'l':'i':'n':' ':cs) = match lex_ids (dropWhile isSpace cs)
    seek lex_ids (c:cs)                    = seek lex_ids cs

    match lex_ids cs =
      let (id,cs1) = break isSpace cs
      in if Set.member id lex_ids
           then case dropWhile isSpace cs1 of
                  ('=':cs2) -> let cs3 = dropWhile isSpace cs2
                                   d   = def 0 cs3
                                   cs4 = drop (length d) cs3
                               in Map.insert id d (seek (Set.delete id lex_ids) cs4)
                  cs2       -> seek lex_ids cs2
           else seek lex_ids cs1

    def c []          = []
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

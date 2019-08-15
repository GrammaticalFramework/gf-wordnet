{-# LANGUAGE CPP, MonadComprehensions #-}

import Database.Helda
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
  mb_s2 <- getInput "access_token"
  mb_s3 <- getInput "check_id"
  mb_s4 <- getInput "lang"
  mb_s5 <- fmap (fmap (urlDecodeUnicode . UTF8.decodeString)) $ getInput "def"
  mb_s6 <- getInput "commit"
  case mb_s1 of
    Just code -> doLogin code
    Nothing   -> case liftM3 (,,) mb_s3 mb_s4 mb_s5 of
                   Just (lex_id,lang,def) -> do json <- liftIO (doCheck lex_id lang def)
                                                outputJSONP json
                   Nothing                -> case liftM2 (,) mb_s2 mb_s6 of
                                               Just (token,commit) -> do res <- liftIO (doCommit token)
                                                                         outputText "text/plain; charset=utf-8" res
                                               Nothing             -> outputNothing
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
                                  resultToEither (valFromObj "login" obj)) of
                           Right user -> do setCookie ((newCookie "access_token" token){cookiePath=Just "/wordnet"})
                                            setCookie ((newCookie "user" user){cookiePath=Just "/wordnet"})
                                            redirect "/wordnet"
                           Left err     -> output err
        Nothing    -> outputNothing

    doCheck lex_id lang def =
      runHelda db ReadWriteMode $ do
        res <- update lexemes (\id -> updateDef lang def) (fromIndexAt lexemes_fun lex_id)
        insert checked (lex_id,lang)
        return [map toLower (show st)
                   | (_,lexeme) <- res, 
                     (lang',def',st) <- lex_defs lexeme,
                     lang==lang', def==def', lex_fun lexeme==lex_id]
      where
        updateDef lang def lexeme =
          lexeme{lex_defs=[if lang==lang'
                             then (lang,def,if def==def'
                                              then Checked
                                              else Changed)
                             else (lang',def',st)
                              | (lang',def',st) <- lex_defs lexeme]}

    doCommit token = do
      res <- runHelda db ReadOnlyMode $
               select [(lang,[(lex_id,[def | (lang',def,_) <- lex_defs lexeme, lang==lang'])]) | 
                            (_,(lex_id,lang)) <- from checked,
                            (_,lexeme) <- fromIndexAt lexemes_fun lex_id]
      forM_ ((Map.toList . Map.fromListWith (++)) res) $ \(lang,ids) ->
        if null ids
          then return ()
          else do let fname = "WordNet"++drop 5 lang++".gf"
                  hInp <- openFile (SERVER_PATH++"/"++fname) ReadMode
                  hSetEncoding hInp utf8
                  ls <- fmap lines $ hGetContents hInp
                  (tmp_fname,hTmp) <- openTempFile SERVER_PATH fname
                  hSetEncoding hTmp utf8
                  mapM_ (hPutStrLn hTmp . annotate ids) ls 
                  hClose hTmp
                  hClose hInp
                  setFileMode tmp_fname (ownerReadMode `unionFileModes`
                                         ownerWriteMode `unionFileModes`
                                         groupReadMode `unionFileModes`
                                         groupWriteMode `unionFileModes`
                                         otherReadMode)
                  renameFile tmp_fname (SERVER_PATH++"/"++fname)
      req0 <- parseRequest ("https://api.github.com/user?access_token="++token)
      let req = req0{requestHeaders=(hUserAgent,BSS.fromString "GF WordNet"):requestHeaders req0}
      man <- liftIO $ newManager tlsManagerSettings
      res <- httpLbs req man
      case (do JSObject obj <- runGetJSON readJSObject (UTF8.toString (responseBody res))
               name  <- resultToEither (valFromObj "name" obj)
               email <- resultToEither (valFromObj "email" obj)
               return (name++" <"++email++">")) of
        Right author -> do (res,out,err) <- 
                               git ["pull","--no-edit"] `thenDo`
                               git ["commit","--author",author,"--message","progress","WordNet*.gf"] `thenDo`
                               git ["push","https://"++token++"@github.com/GrammaticalFramework/gf-wordnet"]
                           return (show res++"\n"++out++"\n"++err)
        Left err     -> return err
      where
        annotate checked l =
          case words l of
            ("lin":id:"=":_) -> case lookup id checked of
                                  Just [def] -> "lin "++id++" = "++def++" ;"
                                  _          -> l
            _                                -> l


git args = readCreateProcessWithExitCode (proc "git" args){cwd=Just SERVER_PATH} ""

thenDo f g =
  do (code1,out1,err1) <- f
     case code1 of
       ExitSuccess -> do (code2,out2,err2) <- g
                         return (code2,out1++out2,err1++err2)
       _           -> return (code1,out1,err1)

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

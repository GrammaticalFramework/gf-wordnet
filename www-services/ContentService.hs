import Text.JSON
import Text.JSON.String
import Network.CGI
import Network.FastCGI(runFastCGI,runFastCGIConcurrent')
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import System.Environment
import qualified Codec.Binary.UTF8.String as UTF8 (encodeString,decodeString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8 (toString,fromString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.UTF8 as BSS

main = do
-- #ifndef mingw32_HOST_OS
--    runFastCGIConcurrent' forkIO 100 (cgiMain db)
-- #else
      runFastCGI (handleErrors $ cgiMain)
-- #endif

cgiMain :: CGI CGIResult
cgiMain = do
  Just code <- getInput "code"
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

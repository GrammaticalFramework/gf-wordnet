{-# LANGUAGE MonadComprehensions #-}
import Network.URI hiding (query)
import Network.HTTP
import System.FilePath
import Control.Exception(bracket,bracket_,catch,throw)
import System.IO (openFile,IOMode(ReadMode),hGetBuf,hFileSize,hClose)
import System.IO.Error(isAlreadyExistsError,isDoesNotExistError)
import System.Directory(getModificationTime,getCurrentDirectory)
import System.Environment(getArgs)
import Foreign(allocaBytes)
import Data.Time (getCurrentTime,formatTime)
import Data.Time.Format(defaultTimeLocale,rfc822DateFormat)
import qualified Data.Map as Map
import Database.Daison
import PGF2
import GF.Compile
import GF.Infra.Option
import SenseService
import ContentService
import FunctionsService hiding (main)
import SenseSchema


main = do
  [doc_dir,client_secret] <- getArgs
  gr <- readNGF (doc_dir</>"Parse.ngf")
  db <- openDB (doc_dir</>"semantics.db")
  bigram_total <- runDaison db ReadOnlyMode $ do
    query sumRows
          [c*c
             | (ex_id,(ex,_)) <- from examples everything
             , let c = length (exprFunctions ex)]
  (_,(mn,sgr)) <- batchCompile noOptions (Just gr) [doc_dir</>"gf/WordNet.gf"]
  server (Just 8080) Nothing (httpMain db gr bigram_total mn sgr doc_dir client_secret)
  closeDB db


httpMain db gr bigram_total mn sgr doc_dir client_secret conn = do
  rq <- receiveHTTP conn
  let path  = uriPath (rqURI rq)
      query = rqQuery rq
  putStrLn $ show (rqMethod rq) ++" "++path++" "++show (map cutSnd query)
  case Map.lookup (takeExtension path) mimeTypes of
    Just mine_type
      | rqMethod rq == PUT -> updateStaticFile mine_type (doc_dir</>tail path) (rqBody rq)
      | otherwise          -> serveStaticFile  mine_type (doc_dir</>tail path)
    Nothing
      | path == "/"-> do respondHTTP conn (Response
                                             { rspCode = 308
                                             , rspReason = "Permanent Redirect"
                                             , rspHeaders = [Header HdrLocation "gf-wordnet.html"]
                                             , rspBody = ""
                                             })
      | path == "/SenseService.fcgi"
                   -> do rsp <- senseService db bigram_total rq
                         respondHTTP conn rsp
      | path == "/ContentService.fcgi"
                    -> do rsp <- contentService db client_secret rq
                          respondHTTP conn rsp
      | path == "/FunctionsService.fcgi"
                   -> do rsp <- functionsService db gr mn sgr rq
                         respondHTTP conn rsp
      | path == "/index.wsgi"
                   -> do rsp <- pageService db gr mn sgr (doc_dir</>"gf-wikidata.wiki") rq
                         respondHTTP conn rsp
      | takeExtension path == ".wiki"
                   -> do rsp <- pageService db gr mn sgr (doc_dir</>tail path) rq
                         respondHTTP conn rsp
      | otherwise  -> respondHTTP conn (Response
                                          { rspCode = 400
                                          , rspReason = "Unsupported file type"
                                          , rspHeaders = []
                                          , rspBody = ""
                                          })
  where
    cutSnd (x,y) = (x,take 500 y)

    mimeTypes = Map.fromList
      [(".html", "text/html; charset=UTF8")
      ,(".css",  "text/css; charset=UTF8")
      ,(".js",   "text/javascript; charset=UTF8")
      ,(".png",  "image/png")
      ,(".svg",  "image/svg+xml; charset=UTF8")
      ,(".ico",  "image/vnd.microsoft.icon")
      ,(".tsv",  "text/tab-separated-values; charset=UTF8")
      ,(".tab",  "text/tab-separated-values; charset=UTF8")
      ,(".json", "application/json; charset=UTF8")
      ,(".gf",   "text/x-gf; charset=UTF8")
      ]

    serveStaticFile mime_type path =
      (bracket (openFile path ReadMode) hClose $ \h -> do
                size <- hFileSize h
                time <- getModificationTime path
                let fmt = formatTime defaultTimeLocale rfc822DateFormat time
                writeHeaders conn (Response
                                      { rspCode = 200
                                      , rspReason = "OK"
                                      , rspHeaders = [Header HdrContentType mime_type
                                                     ,Header HdrContentLength (show size)
                                                     ,Header HdrDate fmt
                                                     ]
                                      , rspBody = ""                                                     
                                      })
                allocaBytes buf_size $ transmit h conn size)
              `catch`
              (\(e :: IOError) ->
                   if isDoesNotExistError e
                     then do cwd <- getCurrentDirectory
                             respondHTTP conn (Response
                                                 { rspCode = 400
                                                 , rspReason = "Not found: "++cwd</>path
                                                 , rspHeaders = []
                                                 , rspBody = ""
                                                 })
                     else throw e)
      where
        buf_size = 4096

        transmit h conn 0    buf = return ()
        transmit h conn size buf = do
          n <- hGetBuf h buf buf_size
          writeBytes conn buf n
          transmit h conn (size-fromIntegral n) buf

    updateStaticFile mime_type path content
      | takeWhile (/=';') mime_type == "text/x-gf" =
          (do writeFile path content
              respondHTTP conn (Response
                                  { rspCode = 200
                                  , rspReason = "OK"
                                  , rspHeaders = []
                                  , rspBody = ""
                                  }))
          `catch`
          (\(e :: IOError) ->
               if isDoesNotExistError e
                 then do cwd <- getCurrentDirectory
                         respondHTTP conn (Response
                                             { rspCode = 400
                                             , rspReason = "Not found: "++cwd</>path
                                             , rspHeaders = []
                                             , rspBody = ""
                                             })
               else throw e)
      | otherwise =
          respondHTTP conn (Response
                              { rspCode = 400
                              , rspReason = "Unsupported file type"
                              , rspHeaders = []
                              , rspBody = ""
                              })

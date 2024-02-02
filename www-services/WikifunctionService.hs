{-# LANGUAGE CPP, MonadComprehensions, BangPatterns #-}

import Network.URI
import Network.HTTP
import Text.JSON
import Text.PrettyPrint
import OpenSSL
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import GF.Compile
import qualified GF.Data.ErrM as E
import GF.Infra.CheckM
import GF.Infra.Option
import GF.Grammar
import GF.Term
import System.FilePath
import Control.Monad.ST.Unsafe
import Control.Monad(foldM)
import PGF2

main = do
  gr <- readNGF "/usr/local/share/x86_64-linux-ghc-8.8.4/gf-4.0.0/www/robust/Parse.ngf"
  (_,(mn,sgr)) <- batchCompile noOptions (Just gr) ["/home/krasimir/GF/gf-wordnet/gf/WordNet.gf"]
  withOpenSSL (simpleServer (Just 8080) Nothing (httpMain gr sgr mn))


httpMain :: PGF -> SourceGrammar -> ModuleName -> Request -> IO Response
httpMain gr sgr mn rq
  | takeExtension path == ".html" = do
      html <- readFile ("../www"++path)
      return (Response
                { rspCode = 200
                , rspReason = "OK"
                , rspHeaders = [Header HdrContentType "text/html; charset=UTF8"]
                , rspBody = html
                })
  | path == "/execute" =
      case decode (rqBody rq) >>= parseQuery of
        Ok f      -> f
        Error msg -> return (Response
                               { rspCode = 400
                               , rspReason = "Invalid input"
                               , rspHeaders = [Header HdrContentType "text/plain; charset=UTF8"]
                               , rspBody = msg
                               })
  | otherwise    =  return (Response
                               { rspCode = 400
                               , rspReason = "Not found"
                               , rspHeaders = []
                               , rspBody = ""
                               })
  where
    path = uriPath (rqURI rq)

    parseQuery query = do
      qid  <- valFromObj "qid"  query
      lang <- valFromObj "lang" query
      code <- valFromObj "code" query
      return (executeCode gr sgr mn qid lang code)

executeCode :: PGF -> SourceGrammar -> ModuleName -> String -> String -> String -> IO Response
executeCode gr sgr mn qid lang code =
  case runP pTerm (BS.pack code) of
    Right term     ->
      case runCheck (checkComputeTerm term) of
        E.Ok (value,msg)
                   -> return (Response
                                { rspCode = 200
                                , rspReason = "OK"
                                , rspHeaders = [Header HdrContentType "text/plain; charset=UTF8"]
                                , rspBody = if null msg
                                              then value
                                              else msg++"\n"++value
                                })
        E.Bad msg  -> return (Response
                                { rspCode = 400
                                , rspReason = "Invalid Expression"
                                , rspHeaders = [Header HdrContentType "text/plain; charset=UTF8"]
                                , rspBody = msg
                                })
    Left (pos,msg) -> return (Response
                                { rspCode = 400
                                , rspReason = "Parse Error"
                                , rspHeaders = [Header HdrContentType "text/plain; charset=UTF8"]
                                , rspBody = (show pos ++ msg)
                                })
  where
    checkComputeTerm term = do
      let globals = Gl sgr (wikiPredef gr)
          term'   = Abs Explicit (identS "qid") term
      term' <- renameSourceTerm sgr mn term'
      (term',typ) <- checkLType globals term' (Prod Explicit identW typeStr typeStr)
      checkWarn (ppTerm Unqualified 0 term')
      checkWarn (ppTerm Unqualified 0 typ)
      normalStringForm globals (App term' (K qid))

wikiPredef :: PGF -> Map.Map Ident ([Value s] -> EvalM s (ConstValue (Value s)))
wikiPredef pgf = Map.fromList
  [ (identS "entity",    \[typ,VStr qid] -> fetch typ qid >>= \v -> return (Const v))
  , (identS "linearize", \[_,v] -> do let Just cnc = Map.lookup "ParseSwe" (languages pgf)
                                      e <- value2expr v
                                      return (Const (VStr (linearize cnc e))))
  ]
  where
    fetch typ qid = do
      rsp <- unsafeIOToEvalM (simpleHTTP (getRequest ("https://www.wikidata.org/wiki/Special:EntityData/"++qid++".json")))
      case decode (rspBody rsp) >>= valFromObj "entities" >>= valFromObj qid >>= valFromObj "claims" of
        Ok obj    -> entity2value (obj :: JSValue)
        Error msg -> evalError (text msg)

    value2expr (GF.Term.VApp (_,f) tnks) = 
      foldM mkApp (EFun (showIdent f)) tnks
      where
        mkApp e1 tnk = do
          v  <- force tnk
          e2 <- value2expr v
          return (EApp e1 e2)

entity2value obj = do
  tnk1 <- newEvaluatedThunk (VStr "hej")
  tnk2 <- newEvaluatedThunk (VStr "do")
  return (VR [(ident2label (identS "P11"), tnk1),(ident2label (identS "P12"), tnk2)])

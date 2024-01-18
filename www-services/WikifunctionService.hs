{-# LANGUAGE CPP, MonadComprehensions, BangPatterns #-}

import Network.HTTP
import Network.FastCGI
import Text.JSON
import Text.PrettyPrint
import OpenSSL
import qualified Data.ByteString.Char8 as BS
import GF.Compile
import qualified GF.Data.ErrM as E
import GF.Infra.CheckM
import GF.Infra.Option
import GF.Grammar
import GF.Term

main = do
  (_,(mn,sgr)) <- batchCompile noOptions ["RGL.gf"]
  withOpenSSL (simpleFastCGI (fcgiMain sgr mn))


fcgiMain :: SourceGrammar -> ModuleName -> Network.FastCGI.Env -> Request -> IO Response
fcgiMain sgr mn env rq = do
  case decode (rqBody rq) >>= parseQuery of
    Ok f      -> f
    Error msg -> return (Response
                           { rspCode = 400
                           , rspReason = "Invalid input"
                           , rspHeaders = [Header HdrContentType "text/plain; charset=UTF8"]
                           , rspBody = msg
                           })
  where
    parseQuery query = do
      qid  <- valFromObj "qid"  query
      lang <- valFromObj "lang" query
      code <- valFromObj "code" query
      return (executeCode sgr mn qid lang code)

executeCode :: SourceGrammar -> ModuleName -> String -> String -> String -> IO Response
executeCode sgr mn qid lang code =
  case runP pTerm (BS.pack code) of
    Right term     ->
      case runCheck (checkComputeTerm term) of
        E.Ok ((term,typ),msg)
                   -> return (Response
                                { rspCode = 200
                                , rspReason = "OK"
                                , rspHeaders = [Header HdrContentType "text/plain; charset=UTF8"]
                                , rspBody = unlines [render (ppTerm Unqualified 0 typ),
                                                     render (ppTerm Unqualified 0 term)]
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
      let term' = Abs Explicit (identS "entity") term
      term' <- renameSourceTerm sgr mn term'
      (term',typ) <- inferLType sgr term'
      term <- normalForm sgr (App term' (R []))
      return (term, typ)

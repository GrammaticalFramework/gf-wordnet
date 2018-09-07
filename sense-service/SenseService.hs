{-# LANGUAGE CPP #-}
import SG
import PGF2
import SenseSchema
import qualified Data.Map as Map
import Control.Monad(foldM)
import Control.Concurrent(forkIO)
import Network.CGI
import Network.FastCGI(runFastCGI,runFastCGIConcurrent')
import qualified Codec.Binary.UTF8.String as UTF8 (encodeString)
import Text.JSON

main = do
  db <- openSG "/home/krasimir/www/semantics.db"
-- #ifndef mingw32_HOST_OS
  -- runFastCGIConcurrent' forkIO 100 (cgiMain db)
-- #else
  runFastCGI (handleErrors $ cgiMain db)
-- #endif
  closeSG db


cgiMain :: SG -> CGI CGIResult
cgiMain db = do	
  lex_ids <- fmap (maybe [] (\s -> [e | w <- words s, Just e <- [readExpr w]])) $ getInput "lexical_ids"
  json <- liftIO (doQuery lex_ids)
  outputJSONP json
  where
    doQuery lex_ids = do
      senses <- foldM (getSense db) Map.empty lex_ids
      return (showJSON (map mkSenseObj (Map.toList senses)))
      where
        mkSenseObj (sense_id,(mb_gloss,domains,lex_ids)) =
          makeObj ([("sense_id",showJSON sense_id)]++
                   maybe [] (\gloss->[("gloss",showJSON gloss)]) mb_gloss++
                   [("lex_ids",showJSON (map (showExpr []) lex_ids))]++
                   [("domains",showJSON (map (showExpr []) domains))])

        getSense db senses lex_id = do
          res <- queryTriple db (Just lex_id) (Just synset) Nothing
          foldM (getGloss db) senses res

        getGloss db senses x@(_,lex_id,_,sense_id_e) = 
          case Map.lookup sense_id senses of
            Just (mb_gloss,domains,lex_ids) -> return (Map.insert sense_id (mb_gloss,domains,lex_id:lex_ids) senses)
            Nothing                         -> do res <- queryTriple db (Just sense_id_e) (Just gloss) Nothing
                                                  let mb_gloss = head ([unStr gloss_str | (_,_,_,gloss_str) <- res]++[Nothing])
                                                  res <- queryTriple db (Just sense_id_e) (Just domain) Nothing
                                                  let domains = [domain_str | (_,_,_,domain_str) <- res]
                                                  return (Map.insert sense_id (mb_gloss,domains,[lex_id]) senses)
          where
            Just (sense_id,[]) = unApp sense_id_e


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
outputStrict ct x | x == x = do setHeader "Content-Type" ct
                                setXO
                                output x
                  | otherwise = fail "I am the pope."

setXO = setHeader "Access-Control-Allow-Origin" "*"

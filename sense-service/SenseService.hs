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
import Data.Maybe(mapMaybe)
import Data.List(sortOn)
import Data.Char

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
      let sorted_senses = (map snd . sortOn fst . map addKey . Map.toList) senses
      return (showJSON (map mkSenseObj sorted_senses))
      where
        mkSenseObj (sense_id,(mb_gloss,synset,lex_ids)) =
          makeObj ([("sense_id",showJSON sense_id)]++
                   maybe [] (\gloss->[("gloss",showJSON gloss)]) mb_gloss++
                   [("synset",showJSON (map (showExpr []) synset))]++
                   [("lex_ids",mkLexObj lex_ids)])

        mkLexObj lex_ids =
          makeObj [(showExpr [] lex_id,mkInfObj domains examples sexamples) | (lex_id,domains,examples,sexamples) <- lex_ids]
          
        mkInfObj domains examples sexamples =
          makeObj [("domains",  showJSON (mapMaybe unStr domains)),
                   ("examples", showJSON (map (showExpr []) examples)),
                   ("secondary_examples", showJSON (map (showExpr []) sexamples))
                  ]

        getSense db senses lex_id = do
          res <- queryTriple db (Just lex_id) (Just synset) Nothing
          foldM (getGloss db) senses res

        getGloss db senses x@(_,lex_id,_,sense_id_e) = do
          res <- queryTriple db (Just lex_id) (Just domain)  Nothing
          let domains  = [domain_str | (_,_,_,domain_str) <- res]

          res <- queryTriple db (Just lex_id) (Just example) Nothing
          let examples = [example | (_,_,_,example) <- res]

          res <- queryTriple db (Just lex_id) (Just secondary_example) Nothing
          let sexamples = [example | (_,_,_,example) <- res]

          case Map.lookup sense_id senses of
            Just (mb_gloss,synset,lex_ids) -> return (Map.insert sense_id (mb_gloss,synset,(lex_id,domains,examples,sexamples):lex_ids) senses)
            Nothing                        -> do res <- queryTriple db (Just sense_id_e) (Just gloss) Nothing
                                                 let mb_gloss = head ([unStr gloss_str | (_,_,_,gloss_str) <- res]++[Nothing])
                                                 res <- queryTriple db Nothing (Just synset) (Just sense_id_e)
                                                 let synset = [lex_id | (_,lex_id,_,_) <- res]
                                                 return (Map.insert sense_id (mb_gloss,synset,[(lex_id,domains,examples,sexamples)]) senses)
          where
            Just (sense_id,[]) = unApp sense_id_e
            
        addKey (sense_id,(mb_gloss,synset,lex_ids)) = (fst (head key_lex_ids), (sense_id,(mb_gloss,synset,map snd key_lex_ids)))
          where
            key_lex_ids = sortOn fst [(toKey lex_id,x) | x@(lex_id,_,_,_) <- lex_ids]

            toKey lex_id = (reverse rid,reverse rcat,read ('0':reverse rn)::Int)
              where
                s0 = reverse (showExpr [] lex_id)
                (rcat,'_':s1) = break (=='_') s0
                (rn,rid) = break (not . isDigit) s1


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

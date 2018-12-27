{-# LANGUAGE CPP, BangPatterns #-}
import SG
import PGF2
import SenseSchema
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Control.Monad(foldM)
import Control.Concurrent(forkIO)
import Network.CGI
import Network.FastCGI(runFastCGI,runFastCGIConcurrent')
import System.Environment
import qualified Codec.Binary.UTF8.String as UTF8 (encodeString)
import Text.JSON
import Data.Maybe(mapMaybe)
import Data.List(sortOn,sortBy)
import Data.Char

main = do
  db <- openSG "/home/krasimir/www/semantics.db"
  args <- getArgs
  case args of
    ["report"] -> doReport db
    _          -> do res <- queryTriple db (Just global) (Just embedding) Nothing
                     let [cs] = map toCS res

                     res <- queryTriple db Nothing (Just embedding) Nothing
                     let funs = Map.fromList (mapMaybe toEmbedding res)
-- #ifndef mingw32_HOST_OS
--                   runFastCGIConcurrent' forkIO 100 (cgiMain db)
-- #else
                     runFastCGI (handleErrors $ cgiMain db (cs,funs))
-- #endif
  closeSG db
  where
    doReport db = do
      res <- queryTriple db Nothing (Just domain) (Just (mkStr "checked"))
      mapM_ putStrLn [showExpr [] lex_id | t@(_,lex_id,_,_) <- res]


cgiMain :: SG -> Embedding -> CGI CGIResult
cgiMain db (cs,funs) = do
  mb_s1 <- getInput "lexical_ids"
  mb_s2 <- getInput "check_id"
  case mb_s1 of
    Just s  -> do let lex_ids = [e | w <- words s, Just e <- [readExpr w]]
                  json <- liftIO (doQuery lex_ids)
                  outputJSONP json
    Nothing -> case mb_s2 >>= readExpr of
                 Just lex_id -> do json <- liftIO (doCheck lex_id)
                                   outputJSONP json
                 Nothing     -> outputNothing
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
          makeObj [(showExpr [] lex_id,mkInfObj domains examples sexamples heads mods rels) | (lex_id,domains,examples,sexamples,heads,mods,rels) <- lex_ids]

        mkInfObj domains examples sexamples heads mods rels =
          makeObj [("domains",  showJSON (mapMaybe unStr domains)),
                   ("examples", showJSON (map (showExpr []) examples)),
                   ("secondary_examples", showJSON (map (showExpr []) sexamples)),
                   ("heads", makeObj heads),
                   ("modifiers", makeObj mods),
                   ("relations", makeObj rels)
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

          let (heads,mods,rels) =
                case Map.lookup (showExpr [] lex_id) funs of
                  Just (hvec,mvec,vec) -> let res1  = take 100 (sortBy (\x y -> compare (fst y) (fst x))
                                                                       [res | (fun,(hvec',mvec',_)) <- Map.toList funs
                                                                            , res <- [(prod hvec cs mvec',Left fun)
                                                                                     ,(prod mvec cs hvec',Right fun)]])
                                              heads = [(fun,showJSON prob) | (prob,Right fun) <- res1]
                                              mods  = [(fun,showJSON prob) | (prob,Left  fun) <- res1]

                                              res2  = take 100 (sortOn fst [(dist vec vec',(fun,vec')) | (fun,(_,_,vec')) <- Map.toList funs])
                                              rels  = [(fun,showJSON (Vector.toList vec)) | (prob,(fun,vec)) <- res2]
                                          in (heads,mods,rels)
                  Nothing              -> ([],[],[])

          case Map.lookup sense_id senses of
            Just (mb_gloss,synset,lex_ids) -> return (Map.insert sense_id (mb_gloss,synset,(lex_id,domains,examples,sexamples,heads,mods,rels):lex_ids) senses)
            Nothing                        -> do res <- queryTriple db (Just sense_id_e) (Just gloss) Nothing
                                                 let mb_gloss = head ([unStr gloss_str | (_,_,_,gloss_str) <- res]++[Nothing])
                                                 res <- queryTriple db Nothing (Just synset) (Just sense_id_e)
                                                 let synset = [lex_id | (_,lex_id,_,_) <- res]
                                                 return (Map.insert sense_id (mb_gloss,synset,[(lex_id,domains,examples,sexamples,heads,mods,rels)]) senses)
          where
            Just (sense_id,[]) = unApp sense_id_e

        prod v1 v2 v3 = Vector.sum (Vector.zipWith3 (\x y z -> x*y*z) v1 v2 v3)

        dist v1 v2 = Vector.sum (Vector.zipWith diff v1 v2)
          where
            diff x y = (x-y)^2

        addKey (sense_id,(mb_gloss,synset,lex_ids)) = (fst (head key_lex_ids), (sense_id,(mb_gloss,synset,map snd key_lex_ids)))
          where
            key_lex_ids = sortOn fst [(toKey lex_id,x) | x@(lex_id,_,_,_,_,_,_) <- lex_ids]

            toKey lex_id = (reverse rid,reverse rcat,read ('0':reverse rn)::Int)
              where
                s0 = reverse (showExpr [] lex_id)
                (rcat,'_':s1) = break (=='_') s0
                (rn,rid) = break (not . isDigit) s1

    doCheck lex_id = do
      inTransaction db $
        insertTriple db lex_id domain (mkStr "checked")
      return ()

type Embedding = (Vector.Vector Double
                 ,Map.Map Fun (Vector.Vector Double,Vector.Vector Double,Vector.Vector Double)
                 )

toCS (_,_,_,cs_e) =
  let Just (_,cs0) = unApp cs_e
      Just !cs = fmap Vector.fromList (mapM unFloat cs0)
  in cs

toEmbedding (_,fun_e,_,pair_e) = do
    let fun = showExpr [] fun_e
    (_,[h_e,m_e]) <- unApp pair_e
    (_,hs0) <- unApp h_e
    (_,ms0) <- unApp m_e
    !hvec <- fmap Vector.fromList (mapM unFloat hs0)
    !mvec <- fmap Vector.fromList (mapM unFloat ms0)
    let !vec  = Vector.zipWith avg hvec mvec
        avg x y = (x+y)/2
    return (fun, (hvec,mvec,vec))

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

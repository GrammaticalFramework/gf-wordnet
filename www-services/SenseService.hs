{-# LANGUAGE CPP, BangPatterns, MonadComprehensions #-}
import PGF2
import Database.Helda
import SenseSchema
import Interval
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Control.Monad(foldM,msum,forM_)
import Control.Concurrent(forkIO)
import Network.CGI
import Network.FastCGI(runFastCGI,runFastCGIConcurrent')
import URLEncoding
import System.Environment
import qualified Codec.Binary.UTF8.String as UTF8 (encodeString,decodeString)
import Text.JSON
import Data.Maybe(mapMaybe, fromMaybe, catMaybes, isNothing)
import Data.List(sortOn,sortBy,delete,intercalate,nub)
import Data.Char

main = do
  db <- openDB "/usr/local/www/gf-wordnet/www/semantics.db"
  args <- getArgs
  case args of
    ["report"] -> doReport db
    _          -> do st <- runHelda db ReadOnlyMode $ do
                             [cs] <- select [cs | (_,cs) <- from coefficients]

                             let norm v = zipWith (\c x -> (c*x) / len) cs v
                                   where
                                     len = sum (zipWith (*) cs v)

                             funs <- fmap Map.fromList $
                                        select [(fun, (hvec,mvec,vec))
                                                  | (_,Embedding fun hvec' mvec') <- from embeddings
                                                  , let !hvec = Vector.fromList hvec'
                                                        !mvec = Vector.fromList mvec'
                                                        !vec  = Vector.fromList (norm hvec' ++ norm mvec')]
                             return (Vector.fromList cs,funs)
-- #ifndef mingw32_HOST_OS
--                   runFastCGIConcurrent' forkIO 100 (cgiMain db)
-- #else
                     runFastCGI (handleErrors $ cgiMain db st)
-- #endif
  closeDB db
  where
    doReport db = do
      res <- runHelda db ReadOnlyMode $ 
               select [(lex_id,lang,[def | (lang',def,_) <- lex_defs lexeme, lang==lang']) | 
                            (_,(lex_id,lang)) <- from checked,
                            (_,lexeme) <- fromIndexAt lexemes_fun lex_id]
      mapM_ (\(lex_id,lang,defs) -> putStrLn (intercalate "\t" (lex_id:lang:defs))) res

cgiMain :: Database -> Embeddings -> CGI CGIResult
cgiMain db (cs,funs) = do
  mb_s1 <- getInput "lexical_ids"
  mb_s2 <- getInput "context_id"
  mb_s3 <- getInput "gloss_id"
  mb_s4 <- getInput "check_id"
  mb_s5 <- getInput "lang"
  mb_s6 <- fmap (fmap (urlDecodeUnicode . UTF8.decodeString)) $ getInput "def"
  mb_s7 <- getInput "generalize_ids"
  case mb_s1 of
    Just s  -> do json <- liftIO (doQuery (words s))
                  outputJSONP json
    Nothing -> case mb_s2 of
                 Just lex_id -> do json <- liftIO (doContext lex_id)
                                   outputJSONP json
                 Nothing     -> case mb_s3 of
                                  Just lex_id -> do json <- liftIO (doGloss lex_id)
                                                    outputJSONP json
                                  Nothing     -> case (mb_s4,mb_s5,mb_s6) of
                                                   (Just lex_id,Just lang,Just def) -> do  json <- liftIO (doCheck lex_id lang def)
                                                                                           outputJSONP json
                                                   _                                -> case mb_s7 of
                                                                                         Just s  -> do json <- liftIO (doGeneralize (words s))
                                                                                                       outputJSONP json
                                                                                         Nothing -> outputNothing
  where
    doQuery lex_ids = do
      senses <- runHelda db ReadOnlyMode $
                  foldM (getSense db) Map.empty lex_ids
      let sorted_senses = (map snd . sortOn fst . map addKey . Map.toList) senses
      return (showJSON (map mkSenseObj sorted_senses))
      where
        getSense db senses lex_id = do
          lexemes <- select (fromIndexAt lexemes_fun lex_id)
          foldM (getGloss db) senses lexemes

        getGloss db senses (_,Lexeme lex_id lex_defs (Just sense_id) domains ex_ids) = do
          examples  <- select [e | ex_id <- anyOf ex_ids, e <- fromAt examples ex_id]
          sexamples <- select [e | (id,e) <- fromIndexAt examples_fun lex_id, not (elem id ex_ids)]

          case Map.lookup sense_id senses of
            Just (gloss,lex_ids) -> return (Map.insert sense_id (gloss,addInfo lex_id (domains,examples,sexamples) lex_ids) senses)
            Nothing              -> do [Synset _ _ _ gloss] <- select (fromAt synsets sense_id)
                                       lex_ids <- select [(lex_id,lex_defs,Nothing) | (_,Lexeme lex_id lex_defs _ _ _) <- fromIndexAt lexemes_synset sense_id]
                                       return (Map.insert sense_id (gloss,addInfo lex_id (domains,examples,sexamples) lex_ids) senses)

        getGloss db senses _ = return senses

        addInfo lex_id info lex_ids = 
          [(lex_id',lex_defs,if lex_id == lex_id' then Just info else mb_info)
              | (lex_id',lex_defs,mb_info) <- lex_ids]

        addKey (sense_id,(gloss,lex_ids)) = (fst (head key_lex_ids), (sense_id,(gloss,map snd key_lex_ids)))
          where
            key_lex_ids = sortOn fst [(toKey lex_id info,x) | x@(lex_id,_,info) <- lex_ids]

            toKey lex_id info = (isNothing info,reverse rid,reverse rcat,read ('0':reverse rn)::Int)
              where
                s0 = reverse lex_id
                (rcat,'_':s1) = break (=='_') s0
                (rn,rid) = break (not . isDigit) s1

    doContext lex_id =
      let (ctxt,rels) =
             case Map.lookup lex_id funs of
               Just (hvec,mvec,vec) -> let res1  = take 200 (sortBy (\x y -> compare (fst y) (fst x))
                                                                    [res | (fun,(hvec',mvec',_)) <- Map.toList funs
                                                                         , res <- [(prod hvec cs mvec',Left fun)
                                                                                  ,(prod mvec cs hvec',Right fun)]])
                                           ctxt = [mkFunProb fun prob | (prob,fun) <- res1]

                                           res2  = take 200 (sortOn fst [(dist vec vec',(fun,vec')) | (fun,(_,_,vec')) <- Map.toList funs])
                                           rels  = [mkFunVec fun (Vector.toList vec) | (dist,(fun,vec)) <- res2]
                                       in (ctxt,rels)
               Nothing              -> ([],[])
      in return (makeObj [("context",   showJSON ctxt),
                          ("relations", showJSON rels)
                         ])
      where
        prod v1 v2 v3 = Vector.sum (Vector.zipWith3 (\x y z -> x*y*z) v1 v2 v3)

        dist v1 v2 = Vector.sum (Vector.zipWith diff v1 v2)
          where
            diff x y = (x-y)^2

        mkFunProb fun prob = 
          case fun of
            Left  fun -> makeObj [("mod", showJSON fun),("prob", showJSON prob)]
            Right fun -> makeObj [("head",showJSON fun),("prob", showJSON prob)]
        mkFunVec  fun vec  = makeObj [("fun",showJSON fun),("vec",  showJSON vec)]

    doGloss lex_id = do
      glosses <- runHelda db ReadOnlyMode $
                    select [gloss s | (_,lex@(Lexeme{synset=Just synset_id})) <- fromIndexAt lexemes_fun lex_id,
                                      s <- fromAt synsets synset_id]
      return (showJSON glosses)

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

    doGeneralize ids = do
      x <- runHelda db ReadOnlyMode $ fmap catMaybes $ do
         select [synset lexeme | fun <- anyOf ids,
                                 (_,lexeme) <- fromIndexAt lexemes_fun fun]

      let up synset_id =
            runHelda db ReadOnlyMode $ fmap head $ do
              select [parents s | s <- fromAt synsets synset_id]

      ids <- findLCA up (nub x)

      fs <- runHelda db ReadOnlyMode $ do
               select [(synset_id,(gloss,lex_ids))
                          | int <- foldl1Q intersection 
                                           [children s | synset_id <- anyOf ids,
                                                         s <- fromAt synsets synset_id],
                            size int < 2000,
                            (s,e) <- anyOf int,
                            (synset_id,Synset offset _ _ gloss) <- fromInterval synsets (Including s) (Including e),
                            lex_ids <- listAll [(lex_fun,lex_defs,Just (domains,examples,sexamples))
                                                   | (_,Lexeme lex_fun lex_defs _ domains ex_ids) <- fromIndexAt lexemes_synset synset_id,
                                                     examples  <- listAll [e | ex_id <- anyOf ex_ids, e <- fromAt examples ex_id],
                                                     sexamples <- listAll [e | (id,e) <- fromIndexAt examples_fun lex_fun, not (elem id ex_ids)]]]

      return (showJSON (map mkSenseObj fs))

    mkSenseObj (sense_id,(gloss,lex_ids)) =
      makeObj [("sense_id",showJSON sense_id)
              ,("gloss",showJSON gloss)
              ,("lex_ids",mkLexObj lex_ids)
              ]

    mkLexObj lex_ids =
      makeObj [(lex_id,mkInfObj lex_defs info) | (lex_id,lex_defs,info) <- lex_ids]

    mkInfObj lex_defs info =
      makeObj (("lex_defs", mkDefsObj lex_defs) :
               case info of
                 Nothing -> []
                 Just (domains,examples,sexamples) -> [
                         ("match", showJSON True),
                         ("domains",  showJSON domains),
                         ("examples", showJSON (map (showExpr []) examples)),
                         ("secondary_examples", showJSON (map (showExpr []) sexamples))
                         ])

    mkDefsObj lex_defs =
      makeObj [(lang,showJSON (def,map toLower (show status))) | (lang,def,status) <- lex_defs]

findLCA :: (Monad m, Ord a) => (a -> m [a]) -> [a] -> m [a]
findLCA up xs = alternate [([x],[]) | x <- xs] [] [] Map.empty
  where
    n = length xs

    alternate []              []  zs set = return zs
    alternate []              ss2 zs set = alternate ss2 []  zs set
    alternate (([],  []):ss1) ss2 zs set = alternate ss1 ss2 zs set
    alternate (([],  ys):ss1) ss2 zs set = alternate ((reverse ys, []):ss1) ss2 zs set
    alternate ((x:xs,ys):ss1) ss2 zs set
      | Map.lookup x set == Just n       = alternate ss1 ((xs,ys):ss2) zs set
      | otherwise                        = do 
          ws <- up x
          (ys,zs,set) <- ascend ws ys zs set
          alternate ss1 ((xs,ys):ss2) zs set

    ascend []     ys zs set = return (ys,zs,set)
    ascend (x:xs) ys zs set
      | c == n           = ascend xs ys   (x:zs) set'
      | otherwise        = ascend xs (x:ys)  zs  set'
      where
        c    = fromMaybe 0 (Map.lookup x set) + 1
        set' = Map.insert x c set

type Embeddings = (Vector.Vector Double
                  ,Map.Map Fun (Vector.Vector Double,Vector.Vector Double,Vector.Vector Double)
                  )

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

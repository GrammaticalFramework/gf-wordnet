{-# LANGUAGE CPP, BangPatterns, MonadComprehensions #-}
import PGF2
import Database.Daison
import SenseSchema
import Interval
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Control.Monad(foldM,msum,forM_)
import Control.Concurrent(forkIO)
import Network.CGI
import Network.FastCGI(runFastCGI,runFastCGIConcurrent')
import qualified Codec.Binary.UTF8.String as UTF8 (encodeString,decodeString)
import Text.JSON
import Data.Maybe(mapMaybe, fromMaybe, catMaybes, isNothing)
import Data.List(sortOn,sortBy,delete,intercalate,nub)
import Data.Char

main = do
  db <- openDB (SERVER_PATH++"/semantics.db")
  st <- runDaison db ReadOnlyMode $ do
          [cs] <- select [cs | (_,cs) <- from coefficients everything]

          let norm v = zipWith (\c x -> (c*x) / len) cs v
                where
                  len = sum (zipWith (*) cs v)

          funs <- fmap Map.fromList $
                    select [(fun, (hvec,mvec,vec))
                              | (_,Embedding fun hvec' mvec') <- from embeddings everything
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

maxResultLength = 500

cgiMain :: Database -> Embeddings -> CGI CGIResult
cgiMain db (cs,funs) = do
  mb_s1 <- getInput "lexical_ids"
  mb_s2 <- getInput "context_id"
  mb_s3 <- getInput "gloss_id"
  mb_s7 <- getInput "generalize_ids"
  mb_s8 <- getInput "list_domains"
  s9    <- fmap (\xs -> [value | ("domain",value) <- xs]) getInputs
  mb_s10<- getInput "list_top_classes"
  mb_s11<- getInput "class_id"
  case mb_s1 of
    Just s  -> do json <- liftIO (doQuery (words s))
                  outputJSONP json
    Nothing -> case mb_s2 of
                 Just lex_id -> do json <- liftIO (doContext lex_id)
                                   outputJSONP json
                 Nothing     -> case mb_s3 of
                                  Just lex_id -> do json <- liftIO (doGloss lex_id)
                                                    outputJSONP json
                                  Nothing     -> case mb_s7 of
                                                   Just s  -> do json <- liftIO (doGeneralize (words s))
                                                                 outputJSONP json
                                                   Nothing -> case mb_s8 of
                                                                Just _  -> do json <- liftIO doListDomains
                                                                              outputJSONP json
                                                                Nothing -> case s9 of
                                                                             (d:ds) -> do json <- liftIO (doDomainQuery d ds)
                                                                                          outputJSONP json
                                                                             _      -> case mb_s10 of
                                                                                         Just _  -> do json <- liftIO doListTopClasses
                                                                                                       outputJSONP json
                                                                                         Nothing -> case mb_s11 of
                                                                                                      Just id -> do json <- liftIO (doClassQuery (read id))
                                                                                                                    outputJSONP json
                                                                                                      Nothing -> outputNothing
  where
    doQuery lex_ids = do
      senses <- runDaison db ReadOnlyMode $
                  foldM (getSense db) Map.empty lex_ids
      let sorted_senses = (sortSenses . Map.toList) senses
      return (makeObj [("total",     showJSON (length lex_ids))
                      ,("retrieved", showJSON (length lex_ids))
                      ,("result",    showJSON (map mkSenseObj sorted_senses))
                      ])
      where
        getSense db senses lex_id = do
          lexemes <- select (fromIndex lexemes_fun (at lex_id))
          foldM getGloss senses lexemes

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
        prod v1 v2 v3 = Vector.sum (Vector.zipWith3 (\x y z -> x*z) v1 v2 v3)

        dist v1 v2 = Vector.sum (Vector.zipWith diff v1 v2)
          where
            diff x y = (x-y)^2

        mkFunProb fun prob = 
          case fun of
            Left  fun -> makeObj [("mod", showJSON fun),("prob", showJSON prob)]
            Right fun -> makeObj [("head",showJSON fun),("prob", showJSON prob)]
        mkFunVec  fun vec  = makeObj [("fun",showJSON fun),("vec",  showJSON vec)]

    doGloss lex_id = do
      glosses <- runDaison db ReadOnlyMode $
                    select [gloss s | (_,lex@(Lexeme{synset=Just synset_id})) <- fromIndex lexemes_fun (at lex_id),
                                      s <- from synsets (at synset_id)]
      return (showJSON glosses)

    doGeneralize ids = do
      x <- runDaison db ReadOnlyMode $ fmap catMaybes $ do
         select [synset lexeme | fun <- anyOf ids,
                                 (_,lexeme) <- fromIndex lexemes_fun (at fun)]

      let up synset_id =
            runDaison db ReadOnlyMode $ fmap head $ do
              select [parents s | s <- from synsets (at synset_id)]

      ids <- findLCA up (nub x)

      fs <- runDaison db ReadOnlyMode $ do
               select [(synset_id,(gloss,lex_ids))
                          | int <- query (foldRows1 intersection)
                                         [children s | synset_id <- anyOf ids,
                                                       s <- from synsets (at synset_id)],
                            size int < 2000,
                            (s,e) <- anyOf int,
                            (synset_id,Synset offset _ _ gloss) <- from synsets (asc ^>= s ^<= e),
                            lex_ids <- select [(lex_fun,status,frame_inf,Just (domains,images,examples,sexamples))
                                                   | (_,Lexeme lex_fun status _ domains images ex_ids fs) <- fromIndex lexemes_synset (at synset_id),
                                                     examples  <- select [e | ex_id <- anyOf ex_ids, e <- from examples (at ex_id)],
                                                     sexamples <- select [e | (id,e) <- fromIndex examples_fun (at lex_fun), not (elem id ex_ids)],
                                                     frame_inf <- select [(name cls,base_class_id f,frame_id)
                                                                            | frame_id <- anyOf fs
                                                                            , f <- from frames (at frame_id)
                                                                            , cls <- from classes (at (base_class_id f))]
                                                     ]]

      return (showJSON (map mkSenseObj fs))

    doListDomains = do
      x <- runDaison db ReadOnlyMode $ do
         select [domain | (domain,_) <- fromList lexemes_domain everything]
      return (showJSON x)

    doDomainQuery d ds = do
      runDaison db ReadOnlyMode $ do
        lexemes0 <- select [res | res@(_,lexeme) <- fromIndex lexemes_domain (at d),
                                  all (flip elem (domains lexeme)) ds]
        let lexemes1 = take maxResultLength lexemes0
        senses <- foldM getGloss Map.empty lexemes1
        let sorted_senses = (sortSenses . Map.toList) senses
        return (makeObj [("total",     showJSON (length lexemes0))
                        ,("retrieved", showJSON (length lexemes1))
                        ,("result",    showJSON (map mkSenseObj sorted_senses))
                        ])

    doListTopClasses = do
      x <- runDaison db ReadOnlyMode $ do
         select [(id,name cls) | (id,cls) <- from classes everything, isNothing (super_id cls)]
      return (showJSON x)
      
    doClassQuery id = do
      x <- runDaison db ReadOnlyMode $ do
         select [mkClassObj cls frames subclasses
                   | cls <- from classes (at id),
                     frames <- select (getFrames id),
                     subclasses <- select (getChildren id)]
      return (showJSON x)
      where
        getChildren super_id =
          [mkClassObj cls frames subclasses 
             | (id,cls)  <- fromIndex classes_super (at super_id),
               frames <- select (getFrames id),
               subclasses <- select (getChildren id)
             ]

        getFrames class_id =
          [(pattern frm,map (lex_fun . snd) lexemes)
             | (id,frm) <- fromIndex frames_class (at class_id),
               lexemes <- select (fromIndex lexemes_frame (at id))]

    getGloss senses (_,Lexeme lex_id status (Just sense_id) domains images ex_ids _) = do
      examples  <- select [e | ex_id <- anyOf ex_ids, e <- from examples (at ex_id)]
      sexamples <- select [e | (id,e) <- fromIndex examples_fun (at lex_id), not (elem id ex_ids)]

      case Map.lookup sense_id senses of
        Just (gloss,lex_ids) -> return (Map.insert sense_id (gloss,addInfo lex_id (domains,images,examples,sexamples) lex_ids) senses)
        Nothing              -> do [Synset _ _ _ gloss] <- select (from synsets (at sense_id))
                                   lex_ids <- select [(lex_id,status,frame_inf,Nothing)
                                                          | (_,Lexeme lex_id status _ _ _ _ fs) <- fromIndex lexemes_synset (at sense_id),
                                                            frame_inf <- select [(name cls,base_class_id f,frame_id)
                                                                                      | frame_id <- anyOf fs
                                                                                      , f <- from frames (at frame_id)
                                                                                      , cls <- from classes (at (base_class_id f))]]
                                   return (Map.insert sense_id (gloss,addInfo lex_id (domains,images,examples,sexamples) lex_ids) senses)
      where
        addInfo lex_id info lex_ids = 
          [(lex_id',status,frames,if lex_id == lex_id' then Just info else mb_info)
              | (lex_id',status,frames,mb_info) <- lex_ids]

    getGloss senses _ = return senses

    sortSenses = map snd . sortOn fst . map addKey
      where
        addKey (sense_id,(gloss,lex_ids)) = (fst (head key_lex_ids), (sense_id,(gloss,map snd key_lex_ids)))
          where
            key_lex_ids = sortOn fst [(toKey lex_id info,x) | x@(lex_id,_,_,info) <- lex_ids]

        toKey lex_id info = (isNothing info,reverse rid,reverse rcat,read ('0':reverse rn)::Int)
          where
            s0 = reverse lex_id
            (rcat,'_':s1) = break (=='_') s0
            (rn,rid) =
              case break (=='_') s1 of
                (s2,'_':s3) -> (takeWhile isDigit (dropWhile isAlpha s2),s3)
                _           -> ("0",s1)

    mkSenseObj (sense_id,(gloss,lex_ids)) =
      makeObj [("sense_id",showJSON sense_id)
              ,("gloss",showJSON gloss)
              ,("lex_ids",mkLexObj lex_ids)
              ]

    mkLexObj lex_ids =
      makeObj [(lex_id,mkInfObj status frames info) | (lex_id,status,frames,info) <- lex_ids]

    mkInfObj status frames info =
      makeObj (("status", mkStatusObj status) :
               ("frames", showJSON frames) :
               case info of
                 Nothing -> []
                 Just (domains,images,examples,sexamples) -> [
                         ("match", showJSON True),
                         ("domains",  showJSON domains),
                         ("images",  showJSON images),
                         ("examples", showJSON (map (showExpr []) examples)),
                         ("secondary_examples", showJSON (map (showExpr []) sexamples))
                         ])

    mkStatusObj status =
      makeObj [(lang,showJSON (map toLower (show s))) | (lang,s) <- status]
      
    mkClassObj cls frames subclasses =
      makeObj [("name",       showJSON (name cls))
              ,("vars",       showJSON (vars cls))
              ,("frames",     showJSON (map mkFrameObj frames))
              ,("subclasses", showJSON subclasses)
              ]

    mkFrameObj (pattern,funs) =
      makeObj [("pattern", showJSON (showExpr [] pattern)),
               ("fun",     showJSON funs)
              ]

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

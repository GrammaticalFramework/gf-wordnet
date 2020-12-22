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
-- #ifndef mingw32_HOST_OS
--                   runFastCGIConcurrent' forkIO 100 (cgiMain db)
-- #else
  runFastCGI (handleErrors $ cgiMain db)
-- #endif
  closeDB db

maxResultLength = 500

cgiMain :: Database -> CGI CGIResult
cgiMain db = do
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
                                                                Nothing -> case map read s9 of
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

    doContext lex_id = do
      runDaison db ReadOnlyMode $ do
        ctxt <- select [mkFunProb (lex_fun lex) d
                          | (_,lex) <- fromIndex lexemes_fun (at lex_id)
                          , (CoOccurrence,id,d) <- anyOf (lex_pointers lex)
                          , lex <- from lexemes (at id)]
        synsets <- select [synset_id
                             | (_,lex) <- fromIndex lexemes_fun (at lex_id)
                             , Just synset_id <- return (synset lex)]
        graph <- foldM (crawlGraph 2) Map.empty synsets
        return (makeObj [("context",  showJSON ctxt)
                        ,("graph",    makeObj [(show key,mkNode node) | (key,node) <- Map.toList graph])
                        ])
      where
        dist v1 v2 = sqrt (Vector.sum (Vector.zipWith diff v1 v2))
          where
            diff x y = (x-y)^2

        mkFunProb fun prob = makeObj [("mod", showJSON fun),("prob", showJSON prob)]

        mkNode (gloss,funs,ptrs) =
          makeObj [("gloss",showJSON gloss)
                  ,("funs",showJSON funs)
                  ,("ptrs",showJSON [(show sym,showJSON id) | (sym,id) <- ptrs])
                  ]

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
              select [[id | (rel,id) <- pointers s
                          , elem rel [Hypernym, InstanceHypernym]]
                              | s <- from synsets (at synset_id)]

      ids <- findLCA up (nub x)

      fs <- runDaison db ReadOnlyMode $ do
               select [(synset_id,(gloss,lex_ids))
                          | int <- query (foldRows intersection [(0,maxBound-1)])
                                         [children s | synset_id <- anyOf ids,
                                                       s <- from synsets (at synset_id)],
                            size int < 2000,
                            (s,e) <- anyOf int,
                            (synset_id,Synset offset _ _ gloss) <- from synsets (asc ^>= s ^<= e),
                            lex_ids <- select [(lex_id,status,frame_inf,Just (domains,images,examples,sexamples,ptrs))
                                                   | (_,Lexeme lex_id status _ domain_ids images ex_ids fs ptrs0) <- fromIndex lexemes_synset (at synset_id),
                                                     domains   <- select [makeObj [ ("id",showJSON domain_id)
                                                                                  , ("name",showJSON (domain_name d))
                                                                                  ]
                                                                            | domain_id <- anyOf domain_ids
                                                                            , d <- from domains (at domain_id)],
                                                     examples  <- select [e | ex_id <- anyOf ex_ids, e <- from examples (at ex_id)],
                                                     sexamples <- select [e | (id,e) <- fromIndex examples_fun (at lex_id), not (elem id ex_ids)],
                                                     ptrs <- select [(sym,lex_fun lex,SenseSchema.status lex) | (sym,id,_) <- anyOf ptrs0, lex <- from lexemes (at id)],
                                                     frame_inf <- select [(name cls,base_class_id f,(frame_id,pattern f,semantics f,Nothing))
                                                                            | frame_id <- anyOf fs
                                                                            , f <- from frames (at frame_id)
                                                                            , cls <- from classes (at (base_class_id f))]
                                                   ]]

      return (showJSON (map mkSenseObj fs))

    doListDomains =
      runDaison db ReadOnlyMode $ do
        roots <- listDomains 0
        return (showJSON roots)
      where
        listDomains :: QueryMonad m => Key Domain -> m [JSValue] 
        listDomains parent =
          select [makeObj [("id",   showJSON id)
                          ,("name", showJSON (domain_name domain))
                          ,("children", showJSON children)
                          ]
                     | (id,domain) <- fromIndex domains_parent (at parent)
                     , children <- listDomains id
                     ]

    doDomainQuery d ds = do
      runDaison db ReadOnlyMode $ do
        lexemes0 <- select [res | res@(_,lexeme) <- fromIndex lexemes_domain (at d),
                                  all (flip elem (domain_ids lexeme)) ds]
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
          [(id,pattern frm,semantics frm,Just (map (lex_fun . snd) lexemes))
             | (id,frm) <- fromIndex frames_class (at class_id),
               lexemes <- select (fromIndex lexemes_frame (at id))]

    getGloss senses (_,Lexeme lex_id status mb_sense_id domain_ids images ex_ids _ ptrs0) = do
      domains   <- select [makeObj [ ("id",showJSON domain_id)
                                   , ("name",showJSON (domain_name d))
                                   ]
                               | domain_id <- anyOf domain_ids
                               , d <- from domains (at domain_id)]
      examples  <- select [e | ex_id <- anyOf ex_ids, e <- from examples (at ex_id)]
      sexamples <- select [e | (id,e) <- fromIndex examples_fun (at lex_id), not (elem id ex_ids)]

      ptrs <- select [(sym,lex_fun lex,SenseSchema.status lex) | (sym,id,_) <- anyOf ptrs0, lex <- from lexemes (at id)]

      case mb_sense_id of
        Just sense_id -> 
          case Map.lookup sense_id senses of
            Just (gloss,lex_ids) -> return (Map.insert sense_id (gloss,addInfo lex_id (domains,images,examples,sexamples,ptrs) lex_ids) senses)
            Nothing              -> do [Synset _ _ _ gloss] <- select (from synsets (at sense_id))
                                       lex_ids <- select [(lex_id,status,frame_inf,Nothing)
                                                              | (_,Lexeme lex_id status _ _ _ _ fs _) <- fromIndex lexemes_synset (at sense_id),
                                                                frame_inf <- select [(name cls,base_class_id f,(frame_id,pattern f,semantics f,Nothing))
                                                                                          | frame_id <- anyOf fs
                                                                                          , f <- from frames (at frame_id)
                                                                                          , cls <- from classes (at (base_class_id f))]]
                                       return (Map.insert sense_id (gloss,addInfo lex_id (domains,images,examples,sexamples,ptrs) lex_ids) senses)
        Nothing -> return (Map.insert (fromIntegral (5000000+Map.size senses)) ("",[(lex_id,status,[],Just (domains,images,examples,sexamples,ptrs))]) senses)
      where
        addInfo lex_id info lex_ids = 
          [(lex_id',status,frames,if lex_id == lex_id' then Just info else mb_info)
              | (lex_id',status,frames,mb_info) <- lex_ids]

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
               ("frames", showJSON [(cid,bcid,mkFrameObj frame) | (cid,bcid,frame) <- frames]) :
               case info of
                 Nothing -> []
                 Just (domains,images,examples,sexamples,ptrs) -> [
                         ("match", showJSON True),
                         ("domains",  showJSON domains),
                         ("images",  showJSON images),
                         ("examples", showJSON (map (showExpr []) examples)),
                         ("secondary_examples", showJSON (map (showExpr []) sexamples)),
                         ("antonyms", makeObj [(id,makeObj [("status", mkStatusObj status)]) | (Antonym,id,status) <- ptrs]),
                         ("derived", makeObj [(id,makeObj [("status", mkStatusObj status)]) | (Derived,id,status) <- ptrs])
                         ])

    mkStatusObj status =
      makeObj [(lang,showJSON (map toLower (show s))) | (lang,s) <- status]

    mkClassObj cls frames subclasses =
      makeObj [("name",       showJSON (name cls))
              ,("vars",       showJSON (vars cls))
              ,("frames",     showJSON (map mkFrameObj frames))
              ,("subclasses", showJSON subclasses)
              ]

    mkFrameObj (id,pattern,semantics,mb_funs) =
      makeObj (("id",        showJSON id) :
               ("pattern",   showJSON (showExpr [] pattern)) :
               ("semantics", showJSON semantics) :
               case mb_funs of
                 Nothing -> []
                 Just funs -> [("fun", showJSON (funs :: [String]))]
              )

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


type Graph   = Map.Map (Key Synset) (String,[Fun],[(PointerSymbol,Key Synset)])

crawlGraph :: Int -> Graph -> Key Synset -> Daison Graph
crawlGraph depth graph synset_id
  | Map.member synset_id graph
                 = return graph
  | depth == 0   = do details <- getDetails False synset_id
                      return (addDetails details graph)
  | otherwise    = do details@(gloss,funs,ptrs) <- getDetails True synset_id
                      foldM (crawlGraph (depth-1)) (addDetails details graph) (map snd ptrs)
  where
    getDetails use_new synset_id = do
      (gloss,ptrs) <- query firstRow
                          [( gloss synset
                           , [ptr | ptr@(sym,tgt) <- pointers synset
                                  , match synset_id tgt]
                           )
                              | synset <- from synsets (at synset_id)
                              ]
      funs <- select [lex_fun lex
                        | (_,lex) <- fromIndex lexemes_synset (at synset_id)]
      return (gloss,funs,ptrs)
      where
        match src tgt =
          case Map.lookup tgt graph of
            Just (_,_,ptrs) -> not (elem src (map snd ptrs))
            Nothing         -> use_new

    addDetails details graph = Map.insert synset_id details graph

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

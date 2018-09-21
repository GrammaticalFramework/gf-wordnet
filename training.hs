{-# LANGUAGE BangPatterns #-}

import PGF2
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char(isDigit)
import Data.Array
import Data.Array.IO
import Data.Array.Unsafe
import Control.Monad(liftM2)
import System.IO
import Debug.Trace

main = do
  putStr "Loading Parse.pgf ..." >> hFlush stdout
  gr <- readPGF "Parse.pgf"
  let null_unigrams = [([f],1) | f <- functions gr]
  putStrLn ""

  putStr "Collect observations ..." >> hFlush stdout
  es <- fmap (concatMap getExpr . lines) $ readFile "examples.txt"
  cfg <- readDepConfig "Parse.labels"
  let (!ex_unigrams,!ex_bigrams) = getExampleStatistics cfg es

  let Just eng = Map.lookup "ParseEng" (languages gr)
  lss <- fmap (splitSentences eng . lines) $ readFile "ud-treebanks-v2.2/UD_English-EWT/en_ewt-ud-train.conllu"
  let !en_unigrams = summarize (concat [concatMap toUnigram ls | ls <- lss])
  let !en_bigrams  = summarize (concat [concatMap (toBigram ls) ls | ls <- lss])

  let Just bul = Map.lookup "ParseBul" (languages gr)
  lss <- fmap (splitSentences bul . lines) $ readFile "ud-treebanks-v2.2/UD_Bulgarian-BTB/bg_btb-ud-train.conllu"
  let !bg_unigrams = summarize (concat [concatMap toUnigram ls | ls <- lss])
  let !bg_bigrams  = summarize (concat [concatMap (toBigram ls) ls | ls <- lss])

  let Just swe = Map.lookup "ParseSwe" (languages gr)
  lss <- fmap (splitSentences swe . lines) $ readFile "ud-treebanks-v2.2/UD_Swedish-Talbanken/sv_talbanken-ud-train.conllu"
  let !sv_unigrams = summarize (concat [concatMap toUnigram ls | ls <- lss])
  let !sv_bigrams  = summarize (concat [concatMap (toBigram ls) ls | ls <- lss])
  putStrLn ""

  let unigrams = null_unigrams ++ ex_unigrams ++ en_unigrams ++ bg_unigrams ++ sv_unigrams
  putStrLn ("Number of unigram sets: "++show (length unigrams))

  let bigrams  = ex_bigrams ++ en_bigrams  ++ bg_bigrams  ++ sv_bigrams
  putStrLn ("Number of bigram sets:  "++show (length bigrams))

  putStrLn "Computing unigrams"
  unigram_ps <- em unigrams
  writeFile "Parse.probs" (unlines [f++"\t"++show p | (f,p) <- mkUnigramProbs gr unigram_ps])

  putStrLn "Computing bigrams"
  bigram_ps  <- em bigrams
  writeFile "bigram.txt" (unlines [x ++ "\t" ++ y ++ "\t" ++ show p | ((x,y),p) <- Map.toList bigram_ps])


getExpr l 
  | take 4 l == "abs:" = case readExpr (drop 5 l) of
                           Just e  -> [e]
                           Nothing -> []
  | otherwise          = []

type Label = String
type DepConfig = Map.Map Fun [(Label,[Pragma])]
data Pragma
       = Anchor
       | DepIndex Int
       | Reverse
       | Relation Label
       deriving Show

readDepConfig :: FilePath -> IO DepConfig
readDepConfig fpath =
  fmap (Map.fromList . concatMap toEntry . lines) $ readFile fpath
  where
    toEntry l =
      case words l of
        []       -> []
        ("--":_) -> []
        (fun:ws) -> [(fun,[toArgument w | w <- ws])]

    toArgument w =
      case words (map (\c -> if c == ':' then ' ' else c) w) of
        (w:ws) -> (w,map toPragma ws)

    toPragma "anchor"            = Anchor
    toPragma "rev"               = Reverse
    toPragma ('r':'e':'l':'=':w) = Relation w
    toPragma w                   = case reads w of
                                     [(n,"")] -> DepIndex n
                                     _        -> error ("Unknown pragma "++w)

-- We want to give a higher weight to statistics from the corpus.
-- For that purpose every observed count is multiplied with
-- the following constant:
example_scale = 5

getExampleStatistics :: DepConfig -> [Expr] -> ([([Fun], Float)], [([(Fun,Fun)], Float)])
getExampleStatistics config es = 
  let (unigrams, bigrams) = foldl' (\stats e -> fst $ getStats stats e) (Map.empty,Map.empty) es
  in ([([f],c*example_scale) | (f,c) <- Map.toList unigrams]
     ,[([(h,m)],c*example_scale) | ((h,_,m),c) <- Map.toList bigrams]
     )
  where
    getStats stats@(unigrams,bigrams) e =
      case unApp e of
        Just (f,[]) -> ((addUni f unigrams,bigrams), [f])
        Just (f,es) -> case Map.lookup f config of
                         Just labels -> let ((unigrams1,bigrams1),heads) = mapAccumL getStats stats es
                                            head                         = concat [mod | ((lbl,pragmas),mod) <- zip labels heads, lbl == "head"]
                                            unigrams2                    = addUni f unigrams1
                                            bigrams2                     = foldl' addDep bigrams1 [cross head lbl mod | ((lbl,pragmas),mod) <- zip labels heads, lbl /= "head"]
                                        in ((unigrams2,bigrams2),head)
                         Nothing     -> error ("Missing configuration for "++f)
        Nothing     -> (stats, [])

    cross head lbl mod = [(h,lbl,m) | h <- head, m <- mod]

    addDep = foldl' (flip (Map.alter (maybe (Just 1) (Just . (+1)))))
    addUni = Map.alter (maybe (Just 1) (Just . (+1)))


splitSentences cnc []   = []
splitSentences cnc (l:ls)
  | take 1 l == "#" = splitSentences cnc ls
  | otherwise       = let (ls1,ls2) = break null ls
                      in map (morpho . tsv) (l:ls1) : 
                         case ls2 of
                           []    -> []
                           _:ls2 -> splitSentences cnc ls2
  where
    morpho fs =
      ((Set.toList . Set.fromList . map fst3 . lookupMorpho cnc) (fs !! 1)
      ,case reads (takeWhile isDigit (fs !! 6)) of
         [(x,"")] -> x
         _        -> if fs !! 6 == "_" then 0 else error (fs !! 6)
      )

    fst3 (x,y,z) = x

tsv :: String -> [String]
tsv "" = []
tsv cs =
  let (x,cs1) = break (=='\t') cs
  in x : if null cs1 then [] else tsv (tail cs1)

toUnigram (ax,root)
  | null ax   = []
  | otherwise = [(ax, 1.0)]

toBigram ls (ax,root)
  | null ax || null ay = []
  | otherwise          = [(liftM2 (,) ax ay, 1.0)]
  where
    ay = if root == 0 then [] else fst (ls !! (root - 1))

summarize :: Ord k => [(k,Float)] -> [(k,Float)]
summarize = Map.toList . Map.fromListWith (+)

--------------------------------------------------------------
-- This function takes the estimated counts for the functions
-- and computes the probabilities P(f | C) and P(C).
-- In the process it also does Laplace smoothing

mkUnigramProbs :: PGF -> Map.Map Fun Float -> [(CId,Float)]
mkUnigramProbs gr f_ps0 =
  let f_ps  = [(f,fromMaybe 0 (Map.lookup f f_ps0) + 1) | f <- functions gr]
      c_ps  = foldl' addCount Map.empty f_ps
      total = Map.foldl (+) 0 c_ps
  in map (toFunProb c_ps) f_ps ++
     map (toCatProb total) (Map.toList c_ps)
  where
    addCount c_ps (f,p) =
      let Just (_, cat, _) = fmap unType (functionType gr f)
      in Map.insertWith (+) cat p c_ps

    toFunProb c_ps (f,p) =
      let Just (_, cat, _) = fmap unType (functionType gr f)
          total            = fromMaybe 0 (Map.lookup cat c_ps)
      in (f,p/total)

    toCatProb total (cat,p) = (cat,p/total)

--------------------------------------------------------------
-- This function takes the estimated counts for the pairs 
-- of functions and computes the probabilities P(f1,f2).
-- There is no smoothing since these probabilities are expected
-- to be used together with a unigram back off. On the other hand
-- we take away very low probability events since those are just
-- artefacts from the roundings.

mkBigramProbs :: Map.Map k Float -> [(k,Float)]
mkBigramProbs cs =
  let (total,ps) = (clip total 0 . sortBy count) (Map.toList cs)
  in ps
  where
    count :: (k,Float) -> (k,Float) -> Ordering
    count (_,c1) (_,c2) = compare c2 c1

    total0     = Map.foldl (+) 0 cs :: Float
    max        = total0 * exp(-kl_limit) :: Float

    clip total sum ((x,p):xs)
      | sum < max             = let (sum',ys) = clip total (sum + p) xs
                                in (sum',(x,p/total) : ys)
    clip total sum _          = (sum,[])

------------------------------------------------------------
-- This function is the core of the algorithm. Here we use
-- expectation maximization to estimate the counts.
-- The first argument is a list of pairs. The first element
-- of every pair is the list of possible keys and the second
-- element is how many times we have seen this list of 
-- possibilities. The result is the estimated count for each
-- of the keys. The estimation continues until convergency, i.e.
-- until the KL divergency is less than kl_limit.
--
-- Note: the code is optimized for memory efficiency since
-- it must work with more that 50 000 000 latent variables.
-- Be careful if you do any changes here!

data K a = K {-# UNPACK #-} !Int a

em :: Ord k => [([k],Float)] -> IO (Map.Map k Float)
em xs = do
  old <- newArray (0,size-1) (1 :: Float)
  new <- newArray (0,size-1) (0 :: Float)
  new <- loop 1 old new (map addIndices xs)
  arr <- unsafeFreeze new
  return (Map.mapWithKey (\k _ -> arr ! Map.findIndex k keySet) keySet)
  where
    keySet = Map.fromList (concatMap (map (flip (,) ()) . fst) xs)
    total  = sum (map snd xs) :: Float

    addIndices (ks,c) = (map (\k -> K (Map.findIndex k keySet) k) ks, c)

    size = Map.size keySet

    loop :: Int -> IOUArray Int Float -> IOUArray Int Float -> [([K k],Float)] -> IO (IOUArray Int Float)
    loop n old new xs = do
      iter xs
      kl <- divergency
      clean
      putStrLn (show n++" "++show kl)
      if abs kl < kl_limit 
        then return new
        else loop (n+1) new old xs
      where
        iter :: [([K k],Float)] -> IO ()
        iter []          = return ()
        iter ((fs,c):xs) = do total <- sumup fs
                              addCount total fs
                              iter xs
          where
            addCount :: Float -> [K k] -> IO ()
            addCount total []           = return ()
            addCount total (K i f : fs) = do
              old_c <- readArray old i
              new_c <- readArray new i
              writeArray new i (new_c + (old_c/total)*c)
              addCount total fs

        sumup = compute 0
          where
            compute :: Float -> [K k] -> IO Float
            compute !s []           = return s
            compute !s ((K i f):fs) = do
              old_c <- readArray old i
              compute (s + old_c) fs

        divergency = sum 0 0
          where
            sum :: Int -> Float -> IO Float
            sum !i !kl
              | i >= size = return kl
              | otherwise = do
                  old_c <- readArray old i
                  new_c <- readArray new i
                  if abs(old_c-new_c)/total > 1e-50
                    then sum (i+1) (kl + (old_c*log(old_c/new_c)/total))
                    else sum (i+1) kl

        clean = set 0
          where
            set :: Int -> IO ()
            set !i
              | i >= size = return ()
              | otherwise = do
                  writeArray old i 0
                  set (i+1)

kl_limit = 5e-8

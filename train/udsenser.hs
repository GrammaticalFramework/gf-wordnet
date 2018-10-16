{-# LANGUAGE BangPatterns #-}

import PGF2
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char(isDigit)
import Control.Monad(liftM2)
import Debug.Trace
import System.IO
import CONLLReader
import EM

main = do
  putStr "Loading Parse.pgf ..." >> hFlush stdout
  gr <- readPGF "../Parse.pgf"
  let null_unigrams = [([f],1) | f <- functions gr]
  putStrLn ""

  putStr "Collect observations ..." >> hFlush stdout
  es <- fmap (concatMap getExpr . lines) $ readFile "../examples.txt"
  cfg <- readDepConfig "../Parse.labels"
  let (!ex_unigrams,!ex_bigrams) = getExampleStatistics cfg es

  let Just eng = Map.lookup "ParseEng" (languages gr)
  dts <- fmap (readDepTrees eng . lines) $ readFile "ud-treebanks-v2.2/UD_English-EWT/en_ewt-ud-train.conllu"
  let !en_unigrams = concatMap depTreeUnigrams dts
  let !en_bigrams  = concatMap depTreeBigrams dts

  let Just bul = Map.lookup "ParseBul" (languages gr)
  dts <- fmap (readDepTrees bul . lines) $ readFile "ud-treebanks-v2.2/UD_Bulgarian-BTB/bg_btb-ud-train.conllu"
  let !bg_unigrams = concatMap depTreeUnigrams dts
  let !bg_bigrams  = concatMap depTreeBigrams dts

  let Just swe = Map.lookup "ParseSwe" (languages gr)
  dts <- fmap (readDepTrees swe . lines) $ readFile "ud-treebanks-v2.2/UD_Swedish-Talbanken/sv_talbanken-ud-train.conllu"
  let !sv_unigrams = concatMap depTreeUnigrams dts
  let !sv_bigrams  = concatMap depTreeBigrams dts
  putStrLn ""

  let unigrams = summarize (null_unigrams ++ ex_unigrams ++ en_unigrams ++ bg_unigrams ++ sv_unigrams)
  putStrLn ("Number of unigram sets: "++show (length unigrams))

  let bigrams  = summarize (ex_bigrams ++ en_bigrams ++ bg_bigrams ++ sv_bigrams)
  putStrLn ("Number of bigram sets:  "++show (length bigrams))

  putStrLn "Computing unigrams"
  (unigram_ps,_) <- em unigrams kl_limit 0
  writeFile "../Parse.probs" (unlines [f++"\t"++show p | (f,p) <- mkUnigramProbs gr unigram_ps])

  putStrLn "Computing bigrams"
  (bigram_ps,total) <- em bigrams kl_limit kl_cut
  writeFile "../Parse.bigram.probs" (unlines [x ++ "\t" ++ y ++ "\t" ++ show p | ((x,y),c) <- bigram_ps, let p = c/total, p > 0])


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

getExampleStatistics :: DepConfig -> [Expr] -> ([([Fun], Float)], [([(Fun,Fun)], Float)])
getExampleStatistics config es = 
  let (unigrams, bigrams) = foldl' (\stats e -> fst $ getStats stats e) (Map.empty,Map.empty) es
  in ([([f],c) | (f,c) <- Map.toList unigrams]
     ,[([(h,m)],c) | ((h,_,m),c) <- Map.toList bigrams]
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


summarize :: Ord k => [(k,Float)] -> [(k,Float)]
summarize = Map.toList . Map.fromListWith (+)

--------------------------------------------------------------
-- This function takes the estimated counts for the functions
-- and computes the probabilities P(f | C) and P(C).
-- In the process it also does Laplace smoothing

mkUnigramProbs :: PGF -> [(Fun,Float)] -> [(CId,Float)]
mkUnigramProbs gr f_ps =
  let c_ps  = foldl' addCount Map.empty f_ps
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

kl_limit = 5e-8
kl_cut   = 5e-3

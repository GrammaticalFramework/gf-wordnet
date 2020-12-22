import PGF2
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

main = do
  gr  <- readPGF "build/ParseAPI.pgf"
  fns <- fmap (Set.fromList . mapMaybe toFun . lines) $ readFile "WordNet.gf"
  ls  <- fmap lines $ readFile "examples.txt"
  let funs = [exprFunctions e
                | l <- ls,
                  take 4 l == "abs:",
                  Just e <- [readExpr (drop 4 l)]
                ]
      unigrams = mkUnigrams gr (mkCounts (concat funs++functions gr))
      bigram   = mkBigrams unigrams
                   (mkCounts
                       (concatMap (\fs -> [(f,g) | f <- fs,
                                                   Set.member f fns,
                                                   g <- fs,
                                                   Set.member g fns,
                                                   f < g]) funs))
  writeFile "Parse.probs" (unlines [x++"\t"++show p | (x,p) <- Map.toList unigrams])
  writeFile "Parse.bigram.probs" (unlines [f++"\t"++g++"\t"++show p | ((f,g),p) <- Map.toList bigram])

mkUnigrams gr cs = Map.union cat_ps fun_ps
  where
    cat_cs = Map.foldlWithKey addCount Map.empty cs
    cat_ps = Map.map (\c -> c/sum cat_cs) cat_cs
    fun_ps = Map.mapMaybeWithKey normalize cs

    addCount cs f c =
      case fmap unType (PGF2.functionType gr f) of
        Just (_, cat, _) -> Map.insertWith (+) cat c cs
        Nothing          -> cs

    normalize f c =
      case fmap unType (PGF2.functionType gr f) >>= \(_, cat, _) -> Map.lookup cat cat_cs of
        Just cc -> Just (c / cc)
        Nothing -> Nothing

mkCounts xs = Map.fromListWith (+) (map (\x -> (x,1)) xs)

mkBigrams unigrams cs = Map.mapMaybeWithKey toPPMI cs
  where
    total  = sum cs

    uni (f,g) =
      case (Map.lookup f unigrams, Map.lookup g unigrams) of
        (Just p1,Just p2) -> p1*p2
        _                 -> 0

    toPPMI x c =
      let ppmi = log ((c/total)/uni x)
      in if ppmi > 3
           then Just ppmi
           else Nothing

toFun l =
  case words l of
    ("fun":id:_) -> Just id
    _            -> Nothing

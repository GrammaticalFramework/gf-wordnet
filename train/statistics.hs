import PGF2
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

main = do
  gr  <- readPGF "build/ParseAPI.pgf"
  ls  <- fmap lines $ readFile "examples.txt"
  let funs = [exprFunctions e
                | l <- ls,
                  take 4 l == "abs:",
                  Just e <- [readExpr (drop 4 l)]
                ]
      (unigrams,ucp_ps) = mkUnigrams gr (mkCounts (concat funs++functions gr))
  writeFile "Parse.probs" (unlines [x++"\t"++show p | (x,p) <- Map.toList unigrams])
  writeFile "Parse.uncond.probs" (unlines [x++"\t"++show p | (x,p) <- Map.toList ucp_ps])

mkUnigrams gr cs = (Map.union cat_ps fun_ps,ucp_ps)
  where
    total  = sum cs
    cat_cs = Map.foldlWithKey addCount Map.empty cs
    cat_ps = Map.map (\c -> c/total) cat_cs
    fun_ps = Map.mapMaybeWithKey normalize cs
    ucp_ps = Map.map (\c -> c/total) cs

    addCount cs f c =
      case fmap unType (PGF2.functionType gr f) of
        Just (_, cat, _) -> Map.insertWith (+) cat c cs
        Nothing          -> cs

    normalize f c =
      case fmap unType (PGF2.functionType gr f) >>= \(_, cat, _) -> Map.lookup cat cat_cs of
        Just cc -> Just (c / cc)
        Nothing -> Nothing

mkCounts xs = Map.fromListWith (+) (map (\x -> (x,1)) xs)

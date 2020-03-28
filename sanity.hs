import PGF2
import qualified Data.Map as Map
import Data.Char

main = do
  gr <- readPGF "Parse.pgf"
  ls <- fmap lines $ readFile "examples.txt"
  writeFile "examples2.txt" (unlines (process gr ls))
  
process gr []                     = []
process gr (l0:l1:l2:l3:l4:l5:ls)
  | take 4 l0 == "abs:"           = l0:(if l1m then l1 else "FIX: "++l1++"\n          "++linearize eng e):(if l2m then l2 else "FIX: "++l2++"\n          "++linearize swe e):(if l3m then l3 else "FIX: "++l3++"\n          "++linearize bul e):l4:l5:process gr ls
  | take 8 l0 == "abs* Phr"       = (if l1m && l2m && l3m then "DONE: "++l0 else l0):l1:l2:l3:l4:l5:process gr ls
  | otherwise                     = l0:l1:l2:l3:l4:l5:process gr ls
  where
    Just e = readExpr (drop 5 l0)

    langs    = languages gr
    Just eng = Map.lookup "ParseEng" langs
    Just swe = Map.lookup "ParseSwe" langs
    Just bul = Map.lookup "ParseBul" langs

    l1m = elem (map toLower (drop 5 l1)) (map (map toLower) (linearizeAll eng e))
    l2m = elem (map toLower (drop 5 l2)) (map (map toLower) (linearizeAll swe e))
    l3m = elem (map toLower (drop 5 l3)) (map (map toLower) (linearizeAll bul e))
process gr (l:ls) = process gr ls

import Data.Char
import Data.List
import System.Directory

main = do
  transl <- fmap (map (toPair . words) . lines) $ readFile "translit.txt"
  let tbl = sortBy (\x y -> compare (fst x) (fst y)) ([(c,conv transl (toLower c)) | c <- [minBound..maxBound], c /= toLower c]++transl)
  ls <- fmap lines $ readFile "levenstein.c"
  let ls1 = takeWhile (/=start_table) ls ++ [start_table]
      ls2 = ["  {/*"++[c]++"*/ "++show (ord c)++", \""++t++"\"}," | (c,t) <- tbl]
      ls3 = dropWhile (/=end_table) ls
  writeFile "levenstein_tmp.c" (unlines (ls1++ls2++ls3))
  renameFile "levenstein_tmp.c" "levenstein.c"

start_table = "// START TABLE"
end_table   = "// END TABLE"

conv transl c =
  case lookup c transl of
    Just t  -> t
    Nothing -> [c]

toPair [[x],y] = (x,y)
toPair x = error (show x)

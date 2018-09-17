import Data.Maybe
import Data.List
import Data.Char
import PGF2
import System.Environment
import System.Directory

main = do
  args    <- getArgs
  checked <- case args of
               []         -> do es <- fmap (catMaybes . map (readExpr . drop 5) . filter (\l -> take 4 l == "abs:") . lines) $ readFile "examples.txt"
                                return (nub (concatMap lemmas es))
               ("-":args) -> return args
  if null checked
    then return ()
    else do ls <- fmap lines $ readFile "WordNet.gf"
            writeFile "WordNet2.gf" (unlines (map (annotate checked) ls))
            renameFile "WordNet2.gf" "WordNet.gf"

lemmas e =
  case unApp e of
    Just (f,[]) -> [f]
    Just (f,es) -> concatMap lemmas es
    _           -> []
  
annotate checked l =
  case break (=='\t') l of
    (xs,'\t':'[':ys) -> case break (==']') ys of
                          (ys,']':zs) -> let domains = map strip (lines (map (\c -> if c == ',' then '\n' else c) ys))
                                             new_domains = if elem id checked then delete "unchecked" domains else domains
                                         in if null new_domains
                                              then xs++"\t"++dropWhile isSpace zs
                                              else xs++"\t["++intercalate ", " new_domains++"]"++zs
                          _          -> l
    _                                -> l
  where
    ("fun":id:_) = words l

strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

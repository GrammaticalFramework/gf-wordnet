import qualified Data.Map as Map
import Data.Graph
import Data.Maybe
import Data.List hiding (union)
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Char
import Interval

main = do
  ls1 <- fmap (drop 29 . lines) $ readFile "data.adj"
  ls2 <- fmap (drop 29 . lines) $ readFile "data.adv"
  ls3 <- fmap (drop 29 . lines) $ readFile "data.noun"
  ls4 <- fmap (drop 29 . lines) $ readFile "data.verb"
  let es      = map process (ls1++ls2++ls3++ls4)
      (g,f,h) = graphFromEdges es
      tbl     = mkIndex (transposeG g)
  mapM_ (printRow f h tbl) (assocs tbl)


process l = (gloss,offs++"-"++tag,hypernyms ws)
  where
    (offs:_:tag0:ws) = words l

    tag | tag0 == "s" = "a"
        | otherwise   = tag0

    hypernyms []                      = [] 
    hypernyms (('@':_):offs:tag:_:ws) = (offs++"-"++tag) : hypernyms ws
    hypernyms (w:ws)                  = hypernyms ws

    gloss0 = (dropWhile isSpace . tail . dropWhile (/='|')) l

    gloss  = let (es,gs)  = partition isExample (parseComment gloss0) 
             in intercalate "; " (map (reverse . dropWhile isSpace . reverse) gs)

    isExample s = not (null s) && head s == '"'

    parseComment ""       = [""]
    parseComment (';':cs) = "":parseComment (dropWhile isSpace cs)
    parseComment ('"':cs) = case break (=='"') cs of
                              (y,'"':cs) -> case parseComment cs of
                                              (x:xs) -> ('"':y++'"':x):xs
                              _          -> case parseComment cs of
                                              (x:xs) -> (       '"':x):xs
    parseComment (c  :cs) = case parseComment cs of
                              (x:xs) -> (c:x):xs


annotate f h (n,xs) (Node v vs) =
  let (offs,_,hs) = f v
      s           = n+1
      ys@(e,_)    = foldl (annotate f h) (s,(offs,n,s,e,catMaybes (map h hs)):xs) vs
  in ys

printRow f h tbl (v,(i,rngs)) = let (gloss,sense_id,parents0) = f v
                                    parents = [fst (tbl ! v) | sense_id <- parents0, v <- maybeToList (h sense_id)]
                                in putStrLn (intercalate "\t" [sense_id,show i,show parents,show rngs,gloss])

mkIndex g = runSTArray (do
  tbl <- newArray (bounds g) (0,[])
  foldM (((.) . (.)) (fmap fst) (go tbl)) 1 (topSort g)
  return tbl)
  where
    go :: STArray s Vertex (Int,[(Int,Int)]) -> Int -> Vertex -> ST s (Int, [(Int,Int)])
    go tbl i v = do
      (j,rngs) <- readArray tbl v
      if j == 0
        then do writeArray tbl v (i,[])
                (j,rngs) <- gos tbl (i+1) [] (g ! v)
                writeArray tbl v (i,rngs)
                return (j,union [(i,i)] rngs)
        else do return (i,union [(j,j)] rngs)

    gos :: STArray s Vertex (Int,[(Int,Int)]) -> Int -> [(Int,Int)] -> [Vertex] -> ST s (Int, [(Int,Int)])
    gos tbl i rngs []     = return (i,rngs)
    gos tbl i rngs (v:vs) = do
      (i',rngs') <- go tbl i v
      gos tbl i' (union rngs' rngs) vs

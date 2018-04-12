import SG
import PGF2
import Data.Char
import Data.List

main = do
  db <- openSG "semantics.db"
  inTransaction db $ do
    ls <- fmap lines $ readFile "WordNet.gf"
    let glosses = [x | Just (fn,gloss) <- map gloss ls, x <- glossTriples fn gloss]
    mapM_ print glosses
    sequence_ [insertTriple db s p o | (s,p,o) <- glosses]
  closeSG db

gloss l = 
  case words l of
    ("fun":fn:_) -> case dropWhile (/='\t') l of
                      '\t':l -> Just (fn,l)
                      _      -> Nothing
    _            -> Nothing

glossTriples fn s =
  (if null gs then [] else [(fn_e,gloss,mkStr (merge gs))])++
  (if null es then [] else [(fn_e,example,(mkStr . noQuotes) e) | e <- es])
  where
    fn_e    = mkApp fn []
    gloss   = mkApp "gloss"   []
    example = mkApp "example" []
    (es,gs) = partition isExample (parseComment s)
    
    noQuotes s
      | head s == '"' && last s == '"' = (init . tail) s
      | otherwise                      = s

parseComment ""       = [""]
parseComment (';':cs) = "":parseComment (dropWhile isSpace cs)
parseComment ('"':cs) = case break (=='"') cs of
                          (y,'"':cs) -> case parseComment cs of
                                          (x:xs) -> ('"':y++'"':x):xs
                          _          -> case parseComment cs of
                                          (x:xs) -> (       '"':x):xs
parseComment (c  :cs) = case parseComment cs of
                          (x:xs) -> (c:x):xs

merge = intercalate "; "

isExample s = not (null s) && head s == '"'

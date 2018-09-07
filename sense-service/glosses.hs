import SG
import PGF2
import Data.Char
import Data.List

main = do
  db <- openSG "semantics.db"
  inTransaction db $ do
    ls <- fmap lines $ readFile "WordNet.gf"
    let glosses = [x | Just (fn,synset,gloss) <- map gloss ls, x <- glossTriples fn synset gloss]
    sequence_ [print t >> insertTriple db s p o | t@(s,p,o) <- glosses]
  closeSG db

gloss l = 
  case words l of
    ("fun":fn:_) -> case break (=='\t') l of
                      (l1,'\t':l2) -> Just (fn,(reverse . take 10 . reverse) l1,l2)
                      _            -> Nothing
    _            -> Nothing

glossTriples fn synset_id s =
  [(fn_e,synset,synid_e)]++
  (if null gs then [] else [(synid_e,gloss,mkStr (merge gs))])++
  (if null es then [] else [(fn_e,example,(mkStr . noQuotes) e) | e <- es])
  where
    synid_e = mkApp synset_id []
    fn_e    = mkApp fn []
    synset  = mkApp "synset"  []
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

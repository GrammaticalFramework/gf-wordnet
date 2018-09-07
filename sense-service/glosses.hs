import SG
import PGF2
import Data.Char
import Data.List
import SenseSchema

main = do
  db <- openSG "semantics.db"
  inTransaction db $ do
    ls <- fmap lines $ readFile "../WordNet.gf"
    let glosses = [x | Just (fn,synset,gloss) <- map parseGloss ls, x <- glossTriples fn synset gloss]
    sequence_ [print t >> insertTriple db s p o | t@(s,p,o) <- glosses]
  closeSG db

parseGloss l = 
  case words l of
    ("fun":fn:_) -> case break (=='\t') l of
                      (l1,'\t':l2) -> Just (fn,(reverse . take 10 . reverse) l1,l2)
                      _            -> Nothing
    _            -> Nothing

glossTriples fn synset_id s =
  [(fn_e,synset,synid_e)]++
  [(fn_e,example,(mkStr . noQuotes) e) | e <- es]++
  (if null gs then [] else [(synid_e,gloss,mkStr (merge gs))])++
  [(synid_e,domain,mkStr d) | d <- ds]
  where
    synid_e = mkApp synset_id []
    fn_e    = mkApp fn []
    (ds,s1) = splitDomains s
    (es,gs) = partition isExample (parseComment s1)

    noQuotes s
      | head s == '"' && last s == '"' = (init . tail) s
      | otherwise                      = s

splitDomains ('[':cs) =
  case break (flip elem ",]") cs of
     (x,',':cs) -> let (xs,cs') = splitDomains cs
                   in (trim x : xs, cs')
     (x,']':cs) -> ([trim x], cs)
     _          -> ([],       cs)
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
splitDomains cs = ([],cs)

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

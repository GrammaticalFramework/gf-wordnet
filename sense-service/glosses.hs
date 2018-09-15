import SG
import PGF2
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import SenseSchema
import System.Directory

main = do
  removeFile "semantics.db"
  db <- openSG "semantics.db"
  inTransaction db $ do
    ls <- fmap lines $ readFile "../WordNet.gf"
    let fundefs = mapMaybe parseGloss ls
    let funids  = Set.fromList (map (\(fn,_,_) -> fn) fundefs)
    let glosses = [x | (fn,synset,gloss) <- fundefs, x <- glossTriples fn synset gloss]
    sequence_ [insertTriple db s p o | t@(s,p,o) <- glosses]
    ls <- fmap lines $ readFile "../examples.txt"
    sequence_ [insertTriple db s p o | t@(s,p,o) <- parseExamples funids ls]
  closeSG db

parseGloss l = 
  case words l of
    ("fun":fn:_) -> case break (=='\t') l of
                      (l1,'\t':l2) -> Just (fn,(reverse . take 10 . reverse) l1,l2)
                      _            -> Nothing
    _            -> Nothing

glossTriples fn synset_id s =
  [(fn_e,synset,synid_e)]++
  (if null gs then [] else [(synid_e,gloss,mkStr (merge gs))])++
  [(fn_e,domain,mkStr d) | d <- ds]
  where
    synid_e = mkApp synset_id []
    fn_e    = mkApp fn []
    (ds,s1) = splitDomains s
    (es,gs) = partition isExample (parseComment s1)

    noQuotes s
      | head s == '"' && last s == '"' = (init . tail) s
      | otherwise                      = s

splitDomains ('[':cs) = split cs
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    
    split cs =
      case break (flip elem ",]") cs of
        (x,',':cs) -> let (xs,cs') = split (dropWhile isSpace cs)
                      in (trim x : xs, dropWhile isSpace cs')
        (x,']':cs) -> let x' = trim x
                      in (if null x' then [] else [x'], cs)
        _          -> ([],       cs)
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


parseExamples funids []                        = []
parseExamples funids (l1:l2:l3:l4:l5:l6:ls)
  | take 4 l1 == "abs:" && take 4 l5 == "key:" =
      let (w:ws) = words (drop 5 l5)
          fns    = take (read w) ws
          ts     = case readExpr (drop 5 l1) of
                     Just e  -> [(mkApp fn [], example, e) | fn <- fns] ++
                                [(mkApp fn [], secondary_example, e) | 
                                        fn <- exprFunctions e,
                                        Set.member fn funids,
                                        not (elem fn fns)]
                     Nothing -> []
      in ts ++ parseExamples funids ls
parseExamples funids (l:ls)                    = parseExamples funids ls

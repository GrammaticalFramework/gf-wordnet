import Data.List (intersperse)
import Text.Printf (printf)
import Data.Map.Strict as M (Map, findWithDefault,fromList)
import System.Environment (getArgs)

lang :: String
lang = "Por"

functionMap :: Map String String
functionMap = M.fromList [
  -- missing Card and Predet because too complicated
  ("A"      ,  "mkA \"%s\" ; "),
  ("A2"     , "mkA2 (mkA \"%s\") noPrep ; "),
  ("AdA"    , "mkAdA \"%s\" ; "),
  ("AdN"    , "mkAdN \"%s\" ; "),
  ("AdV"    , "mkAdV \"%s\" ; "),
  ("Adv"    , "mkAdv \"%s\" ; "),
  ("CN"     , "UseN (mkN \"%s\") ; "),
  ("Interj" , "ss \"%s\" ; "),
  ("N"      , "mkN \"%s\" ; "),
  ("N2"     , "mkN2 (mkN \"%s\") noPrep ; "),
  ("PN"     , "mkPN \"%s\" ; "),
  ("Prep"   , "mkPrep \"%s\" ; "),
  ("V"      , "mkV \"%s\" ; "),
  ("V2"     , "mkV2 (mkV \"%s\") ; "),
  ("V2A"    , "mkV2A (mkV \"%s\") ; "),
  ("V2S"    , "mkV2S (mkV \"%s\") ; "),
  ("V2V"    , "mkV2V (mkV \"%s\") ; "),
  ("V3"     , "mkV3 (mkV \"%s\") ; "),
  ("VA"     , "mkVA (mkV \"%s\") ; "),
  ("VQ"     , "mkVQ (mkV \"%s\") ; "),
  ("VS"     , "mkVS (mkV \"%s\") ; "),
  ("VV"     , "mkVV (mkV \"%s\") ; "),
  ("Voc"    , "VocNP (MassNP (UseN (mkN \"%s\"))) ; ")
  ]

splitOnElemRight :: Eq a => a -> [a] -> ([a],[a])
splitOnElemRight e = split [] . reverse
  where
    split xs [] = (xs, [])
    split xs (z:zt) = if z == e
                      then (reverse zt, xs)
                      else split (z:xs) zt

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn e = split []
  where
    split [] [] = []
    split xs [] = [reverse xs]
    split xs (y:yt) = if y == e
                      then if null xs
                           then split [] yt
                           else (reverse xs) : split [] yt
                      else split (y:xs) yt

lines2gf :: String -> String
lines2gf l = gf
  where
    absname:forms = splitOn '\t' l
    (abs,cat) = splitOnElemRight '_' absname
    gf = concat ["lin ", absname, " = ", body, "--*\n"]
    body = case forms of
             [] -> "variants {} ; "
             [f] -> mkBody f
             _ -> concat ["variants {", concatMap mkBody forms, "} ; "]
    mkBody = printf $ M.findWithDefault "%.0svariants {} ; " cat functionMap

main :: IO ()
main = do
  [i,o] <- getArgs
  ls <- readFile i
  writeFile o $ concat [header, concatMap lines2gf $ lines ls]
  appendFile o "\n\n} ;\n"
    where
      header = printf "concrete WordNet%s of WordNet = Cat%s ** open Construction%s, Grammar%s, Paradigms%s, Prelude in {\n\nflags\n  coding=utf8 ;\n\n" lang lang lang lang lang

import qualified Data.Map as Map
import Data.List
import Data.Char
import PGF2
import System.Directory

main = do
  putStrLn "Collecting titles"
  fs <- fmap (Map.fromListWith (++) . concatMap toEntry . lines)
             (readFile "images.txt")
  ts <- fmap (map tsv . lines)
             (readFile "data/titles.txt")
  let ifts = foldl' combine (fmap (flip (,) []) fs) ts
      fts  = [(f,tss) |  (_,(fs,tss)) <- Map.toList ifts, f <- fs]
  putStrLn (show (length fts) ++ " titles found")
  putStrLn "Loading grammar"
  gr <- readPGF "Parse.pgf"
  let langs  = tail (Map.toList (languages gr))
      status = map (check langs) fts
  putStrLn "Checking"
  putStrLn (show (sum (map (length . snd) status)) ++ " functions found")
  mapM_ (patchFile (Map.fromList status)) langs

patchFile status (lang,_) = do
  let name = "WordNet"++drop 5 lang++".gf"
      tmp  = "__temp__"
  putStrLn ("Patching "++name)
  ls <- fmap lines $ readFile name
  writeFile tmp (unlines (map patch ls))
  renameFile tmp name
  where
    patch l =
      case words l of
        ws@("lin":id:_) | maybe False (elem lang) (Map.lookup id status) -> cleanup [] l
        _ -> l

    cleanup xs []          = reverse (dropWhile isSpace xs)
    cleanup xs ('-':'-':_) = reverse (dropWhile isSpace xs)
    cleanup xs (y:ys)      = cleanup (y:xs) ys

combine fts (s:ts) = Map.alter (fmap add) id fts
  where
    id = read s

    add (fs,tss) = (fs,ts:tss)

check langs (f,tss) = (f,match langs (transpose tss))
  where
    match []                 _        = []
    match _                  []       = []
    match ((lang,cnc):langs) (ts:tss)
      | null [g | t <- ts, (g,_,_) <- lookupMorpho_ cnc t, f==g] =        match langs tss
      | otherwise                                                = lang : match langs tss
      
    lookupMorpho_ cnc "" = []
    lookupMorpho_ cnc s  =
      case lookupMorpho cnc s of
        [] -> lookupMorpho cnc (map toLower s)
        xs -> xs

toEntry l =
  let id:refs = tsv l
  in [(page_id,[id]) | page_id <- map (read . head . cosv) refs :: [Int]]

tsv :: String -> [String]
tsv "" = []
tsv cs =
  let (x,cs1) = break (=='\t') cs
  in x : if null cs1 then [] else tsv (tail cs1)

cosv :: String -> [String]
cosv "" = []
cosv cs =
  let (x,cs1) = break (==';') cs
  in x : if null cs1 then [] else cosv (tail cs1)

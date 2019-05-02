{-# LANGUAGE OverloadedStrings #-}

import PGF2
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.List
import Data.Char
import Database.SQLite.Simple
import System.IO
import System.Environment
import Text.EditDistance -- pkg edit-distance
import Transliteration
import Morpho

main = do
  args <- getArgs
  case args of
    []     -> predictions
    [lang] -> apply lang

predictions = do
  conn <- status "Open data/panlex.db" $ open "data/panlex.db"
  gr <- status "Read Parse.pgf" $ readPGF "Parse.pgf"
  let langs = Map.toList (languages gr)
  status "Searching for extensions" $ do
    transl <- readTransliteration "bootstrap/translit.txt" 
    sequence [allLins conn transl langs f | f <- functions gr,
                                     ([],_,_) <- maybeToList (fmap unType (functionType gr f))]
  close conn

allLins conn transl langs f = do
  lins <- sequence [getLin f lang langvar concr | (lang,concr) <- langs,
                                                  Just langvar <- [lookup lang langvars]]
  lins <- enrich conn transl lins
  sequence_ [putStrLn (f++"\t"++lang++"\t"++expr++"\t"++show l++"\t"++show t++"\t"++show (-d)) | (lang,exprs,l,t,d) <- lins, t > 0, (_,expr) <- exprs]
  where
    getLin f lang langvar concr
      | hasLinearization concr f = do idss <- mapM (getId langvar) (linearizeAll concr (mkApp f []))
                                      return (Right (lang,concat idss))
      | otherwise                = return (Left (lang,langvar))

    getId langvar lin = do
      res <- query conn "select id from expr where langvar=? and txt=?" (langvar,lin) :: IO [Only Int]
      return [(id,lin) | Only id <- res]

enrich conn transl lins = do
  new_lins <- fmap (rank . concat) (mapM retrive ids)
  return (map (extend new_lins) lins)
  where
    ids  = [id         | Right (_,exprs) <- lins, (id,_ ) <- exprs]
    txts = [transl txt | Right (_,exprs) <- lins, (_,txt) <- exprs]

    retrive id = do
      xs <- query conn "select e.langvar,e.id,e.txt \
                       \from denotation d1 \
                       \join denotation d2 on d1.expr=? and d1.meaning=d2.meaning \
                       \join expr as e on e.id=d2.expr" (Only id) :: IO [(Int,Int,String)]
      return [(x,[id]) | x <- xs]

    rank = Map.toList . Map.mapWithKey counts . Map.fromListWith (++)
      where
        counts (_,_,expr) ids = (length (nub ids),length ids,-minimum (map (dist (transl expr)) txts))

        dist x y = levenshteinDistance defaultEditCosts x y

    takeBest xs = 
      let max_t = maximum ((0,0,0):map snd xs)
      in ([x | (x,t) <- xs, t==max_t],max_t)

    extend new_lins (Right (lang,exprs  )) = (lang,exprs,0,0,0)
    extend new_lins (Left  (lang,langvar)) = 
      let (exprs,(l,t,d)) = takeBest [((id,expr),ltd) | ((langvar',id,expr),ltd) <- new_lins, langvar'==langvar]
      in (lang,exprs,l,t,d)

status msg f = do
  hPutStr stderr (msg++" ...")
  hFlush stderr
  r <- f
  hPutStrLn stderr ""
  return r

langvars :: [(ConcName,Int)]
langvars =
  [("ParseBul",93)
  ,("ParseCat",101)
  ,("ParseChi",1627)
  ,("ParseEng",187)
  ,("ParseEst",190)
  ,("ParseFin",204)
  ,("ParseIta",304)
  ,("ParsePor",579)
  ,("ParseSlv",649)
  ,("ParseSpa",666)
  ,("ParseSwe",691)
  ,("ParseTur",738)
  ]


apply lang = do
  dict <- fmap (Map.fromList . concatMap (toPair lang . tsv) . lines) $ readFile "predictions.tsv"
  let fname = ("WordNet"++(toUpper (head lang):tail lang))
  ls <- fmap lines $ readFile (fname++".gf")
  morphoMap <- readMorpho lang
  writeFile (fname++"2.gf") (unlines (map (annotate morphoMap dict) ls))

toPair lang [id,cnc,w,_,_,_]
  | cnc == "Parse"++(toUpper (head lang):tail lang) = [(id,[w])]
  | otherwise                                       = []

annotate morphoMap dict l =
  case words l of
    ("lin":id:_) -> case Map.lookup id dict of
                      Just t  -> "lin "++id++" = "++prediction2gf "guessed" morphoMap dict id
                      Nothing -> l
    _                         -> l

tsv :: String -> [String]
tsv "" = []
tsv cs =
  let (x,cs1) = break (=='\t') cs
  in x : if null cs1 then [] else tsv (tail cs1)

{-# LANGUAGE OverloadedStrings #-}

import PGF2
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.List
import Database.SQLite.Simple
import System.IO
import Text.EditDistance -- pkg edit-distance
import Transliteration

main = do
  conn <- status "Open data/panlex.db" $ open "data/panlex.db"
  gr <- status "Read Parse.pgf" $ readPGF "Parse.pgf"
  let langs = Map.toList (languages gr)
  lins <- status "Searching for extensions" $ 
            sequence [allLins conn langs f | f <- functions gr,
                                             ([],_,_) <- maybeToList (fmap unType (functionType gr f))]
  status "Computing predictions" $ do
    transl <- readTransliteration "bootstrap/translit.txt"
    let ps = predictions transl lins
    writeFile "predictions.tsv" (unlines [f++"\t"++
                                          lang++"\t"++
                                          expr++"\t"++
                                          show t++"\t"++
                                          show c++"\t"++
                                          show d | (f,lang,expr,t,c,d) <- ps])
  close conn

allLins conn langs f = do
  lins <- sequence [getLin f lang langvar concr | (lang,concr) <- langs,
                                                  Just langvar <- [lookup lang langvars]]
  lins <- enrich conn lins
  return (f,lins)
  where
    getLin f lang langvar concr
      | hasLinearization concr f = do idss <- mapM (getId langvar) (linearizeAll concr (mkApp f []))
                                      return (lang,langvar,concat idss,True)
      | otherwise                = return (lang,langvar,[],False)

    getId langvar lin = do
      res <- query conn "select id from expr where langvar=? and txt=?" (langvar,lin) :: IO [Only Int]
      return [((id,lin),0) | Only id <- res]

enrich conn lins = do
  new_lins <- fmap (rank . concat) (mapM retrive ids)
  return [(lang,langvar,extend flag langvar new_lins exprs,flag) | (lang,langvar,exprs,flag) <- lins]
  where
    ids = concat [map (fst.fst) exprs | (_,_,exprs,_) <- lins]

    retrive id =
      query conn "select e.langvar,e.id,e.txt \
                 \from denotation d1 \
                 \join denotation d2 on d1.expr=? and d1.meaning=d2.meaning \
                 \join expr as e on e.id=d2.expr" (Only id) :: IO [(Int,Int,String)]

    rank xs = (Map.toList . Map.fromListWith (+)) [(x,1) | x <- xs]

    takeBest xs = [(x,c) | (x,c) <- xs, c==maximum (map snd xs)]

    extend True  langvar new_lins exprs = exprs
    extend False langvar new_lins _     = takeBest [((id,expr),c) | ((langvar',id,expr),c) <- new_lins, langvar'==langvar] 

predictions transl lins =
  [(f,lang,snd expr,t,c,d)
          | (f,lins) <- lins,
            (lang,_,exprs,False) <- lins,
            (expr,t) <- exprs,
            let (c,d) = counts lins expr]
  where
    join xs ys = [((x,y),1) | x <- xs, y <- ys]
    dist x y = levenshteinDistance defaultEditCosts (transl x) (transl y)

    cmap  = (Map.fromListWith (+) . concat)
                 [join [expr | (_,_,exprs,True ) <- lins, (expr,_) <- exprs]
                       [expr | (_,_,exprs,False) <- lins, (expr,_) <- exprs] 
                    | (f,lins) <- lins]
    cdmap = Map.mapWithKey (\((_,expr1),(_,expr2)) c -> (c,dist expr1 expr2)) cmap

    counts lins expr2 =
      let (cs,ds) = unzip [fromMaybe (0,0) (Map.lookup (expr1,expr2) cdmap) | (lang,_,exprs,True) <- lins, (expr1,_) <- exprs]
      in (sum cs,minimum (maxBound:ds))

status msg f = do
  hPutStr stderr (msg++" ...")
  hFlush stderr
  r <- f
  hPutStrLn stderr ""
  return r

langvars :: [(ConcName,Int)]
langvars =
  [("ParseBul",93)
  ,("ParseChi",1627)
  ,("ParseEng",187)
  ,("ParseEst",190)
  ,("ParseFin",204)
  ,("ParsePor",579)
  ,("ParseSlv",649)
  ,("ParseSpa",666)
  ,("ParseSwe",691)
  ,("ParseTur",738)
  ]

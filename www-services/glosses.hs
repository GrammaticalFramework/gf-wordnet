{-# LANGUAGE MonadComprehensions, BangPatterns #-}
import PGF2
import Database.Daison
import SenseSchema
import Data.Char
import Data.List(partition,intercalate)
import Data.Maybe
import Data.Data
import System.Directory
import Control.Monad
import qualified Data.Map.Strict as Map

main = do
  cncdefs1 <- fmap (mapMaybe (parseCncSyn "ParseBul") . lines) $ readFile "WordNetBul.gf"
  cncdefs2 <- fmap (mapMaybe (parseCncSyn "ParseCat") . lines) $ readFile "WordNetCat.gf"
  cncdefs3 <- fmap (mapMaybe (parseCncSyn "ParseChi") . lines) $ readFile "WordNetChi.gf"
  cncdefs4 <- fmap (mapMaybe (parseCncSyn "ParseDut") . lines) $ readFile "WordNetDut.gf"
  cncdefs5 <- fmap (mapMaybe (parseCncSyn "ParseEng") . lines) $ readFile "WordNetEng.gf"
  cncdefs6 <- fmap (mapMaybe (parseCncSyn "ParseEst") . lines) $ readFile "WordNetEst.gf"
  cncdefs7 <- fmap (mapMaybe (parseCncSyn "ParseFin") . lines) $ readFile "WordNetFin.gf"
  cncdefs8 <- fmap (mapMaybe (parseCncSyn "ParseIta") . lines) $ readFile "WordNetIta.gf"
  cncdefs9 <- fmap (mapMaybe (parseCncSyn "ParsePor") . lines) $ readFile "WordNetPor.gf"
  cncdefs10<- fmap (mapMaybe (parseCncSyn "ParseSlv") . lines) $ readFile "WordNetSlv.gf"
  cncdefs11<- fmap (mapMaybe (parseCncSyn "ParseSpa") . lines) $ readFile "WordNetSpa.gf"
  cncdefs12<- fmap (mapMaybe (parseCncSyn "ParseSwe") . lines) $ readFile "WordNetSwe.gf"
  cncdefs13<- fmap (mapMaybe (parseCncSyn "ParseTha") . lines) $ readFile "WordNetTha.gf"
  cncdefs14<- fmap (mapMaybe (parseCncSyn "ParseTur") . lines) $ readFile "WordNetTur.gf"

  let cncdefs = Map.fromListWith (++) (cncdefs1++cncdefs2++cncdefs3++cncdefs4++cncdefs5++cncdefs6++cncdefs7++cncdefs8++cncdefs9++cncdefs10++cncdefs11++cncdefs12++cncdefs13++cncdefs14)

  absdefs <- fmap (mapMaybe parseAbsSyn . lines) $ readFile "WordNet.gf"

  fn_examples <- fmap (parseExamples . lines) $ readFile "examples.txt"

  taxonomy <- fmap (map parseTaxonomy . lines) $ readFile "taxonomy.txt"

  ls <- fmap lines $ readFile "embedding.txt"
  let (cs,ws) = parseEmbeddings ls

  ls <- fmap lines $ readFile "images.txt"
  let images = parseImages ls

  let db_name = "semantics.db"
  fileExists <- doesFileExist db_name
  when fileExists (removeFile db_name)
  db <- openDB db_name
  runDaison db ReadWriteMode $ do
    createTable examples
    ex_keys <- fmap (Map.fromListWith (++) . concat) $ forM fn_examples $ \(fns,e) -> do
                 key <- insert_ examples e
                 return [(fn,[key]) | fn <- fns]

    createTable synsets
    forM taxonomy $ \(key,synset) -> do
       store synsets (Just key) synset

    createTable lexemes
    let synsetKeys = Map.fromList [(synsetOffset synset, key) | (key,synset) <- taxonomy]
    forM absdefs $ \(mb_offset,fun,ds) -> do
       insert_ lexemes (Lexeme fun 
                               (Map.findWithDefault [] fun cncdefs)
                               (mb_offset >>= flip Map.lookup synsetKeys)
                               ds
                               (fromMaybe [] (Map.lookup fun images))
                               (fromMaybe [] (Map.lookup fun ex_keys)))

    createTable coefficients
    insert_ coefficients cs

    createTable embeddings
    mapM_ (insert_ embeddings) ws
    
    createTable updates

  [cs] <- runDaison db ReadOnlyMode $ 
            select $ 
              foldlQ accumCounts Map.empty $
                [(drop 5 lang,status)
                           | (_,lex) <- from lexemes everything,
                             (lang,status) <- anyOf (status lex)]
  writeFile "build/status.svg" (renderStatus cs)

  closeDB db

parseAbsSyn l =
  case words l of
    ("fun":fn:_) -> case break (=='\t') l of
                      (l1,'\t':l2) -> let (ds,l3) = splitDomains l2
                                      in Just (Just ((reverse . take 10 . reverse) l1), fn, ds)
                      _            -> Just (Nothing, fn, [])
    _            -> Nothing
  where
    splitDomains ('[':cs) = split cs
      where
        trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

        split cs =
          case break (flip elem ",]") cs of
            (x,',':cs) -> let (xs,cs') = split (dropWhile isSpace cs)
                          in (trim x : xs, dropWhile isSpace cs')
            (x,']':cs) -> let x' = trim x
                          in (if null x' then [] else [x'], dropWhile isSpace cs)
            _          -> ([],       cs)
    splitDomains cs = ([],cs)

parseCncSyn lang l =
  case words l of
    ("lin":fn:"=":ws) | null ws                  -> Nothing
                      | last ws == "--unchecked" -> Just (fn,[(lang,Unchecked) | def <- strip (unwords (init ws))])
                      | last ws == "--guessed"   -> Just (fn,[(lang,Guessed)   | def <- strip (unwords (init ws))])
                      | otherwise                -> Just (fn,[(lang,Checked)   | def <- strip (unwords ws)])
    _                                            -> Nothing
  where
    strip s = 
      let def = (reverse . dropWhile (\c -> isSpace c || c == ';') . reverse) s
      in if def == "variants {}"
           then []
           else [def]

parseExamples []                        = []
parseExamples (l1:l2:l3:l4:l5:l6:ls)
  | take 4 l1 == "abs:" && take 4 l5 == "key:" =
      let (w:ws) = words (drop 5 l5)
          fns    = take (read w) ws
          ts     = case readExpr (drop 5 l1) of
                     Just e  -> [(fns, e)]
                     Nothing -> []
      in ts ++ parseExamples ls
parseExamples (l:ls)                    = parseExamples ls

parseTaxonomy l =
  (read key_s :: Key Synset, Synset offset (read parents_s) (read children_s) gloss)
  where
    [offset,key_s,parents_s,children_s,gloss] = tsv l

parseEmbeddings (l:"":ls) = (parseVector l, parseWords ls)
  where
    parseWords []               = []
    parseWords (l1:l2:l3:"":ls) = 
      let hvec = parseVector l2
          mvec = parseVector l3
      in sum hvec `seq` sum mvec `seq` (Embedding l1 hvec mvec):parseWords ls

    parseVector = map read . words :: String -> [Double]

parseImages ls = 
  Map.fromList [case tsv l of {(id:urls) -> (id,map (\s -> case cosv s of {[_,pg,im] -> (pg,im); _ -> error l}) urls)} | l <- ls]

accumCounts m (lang,status) = Map.alter (Just . add) lang m
  where
    add Nothing                = (0,0,0,0)
    add (Just (!g,!u,!ca,!ce)) = case status of
                                   Guessed   -> (g+0.001,u,ca,ce)
                                   Unchecked -> (g,u+0.001,ca,ce)
                                   Changed   -> (g,u,ca+0.001,ce)
                                   Checked   -> (g,u,ca,ce+0.001)

renderStatus cs =
      let (s1,x,y) = Map.foldlWithKey renderBar  ("",5,0) cs
          (s2,_,_) = Map.foldlWithKey renderLang ("",5,y) cs
      in "<?xml version=\"1.0\" encoding=\"utf-8\"?>"++
         "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""++show x++"\" height=\""++show (y+20)++"\">\n"++
         "<g transform=\"translate(0,"++show y++") scale(1,-1)\">\n"++
         s1++
         "</g>\n"++
         s2++
         "</svg>"
      where
        renderBar (s,x,y) lang (g,u,ca,ce) =
          let bar =
                "<rect x=\""++show x++"\" y=\""++show 0++"\" width=\"30\" height=\""++show g++"\" style=\"fill:red\"/>\n"++
                "<rect x=\""++show x++"\" y=\""++show g++"\" width=\"30\" height=\""++show u++"\" style=\"fill:yellow\"/>\n"++
                "<rect x=\""++show x++"\" y=\""++show (g+u)++"\" width=\"30\" height=\""++show ca++"\" style=\"fill:black\"/>\n"++
                "<rect x=\""++show x++"\" y=\""++show (g+u+ca)++"\" width=\"30\" height=\""++show ce++"\" style=\"fill:green\"/>\n"
          in (bar++s,x+35,max y (g+u+ca+ce))

        renderLang (s,x,y) lang (g,u,ca,ce) =
          let text =
                "<text x=\""++show (x+3)++"\" y=\""++show (y+15)++"\">"++lang++"</text>"
          in (text++s,x+35,y)

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

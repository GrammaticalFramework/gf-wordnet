{-# LANGUAGE OverloadedStrings, BangPatterns #-}

import PGF2
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List(foldl',sortBy,intercalate)
import Data.Maybe(fromMaybe,fromJust,mapMaybe)
import Data.Char(toLower)
import System.IO
import System.Directory
import Database.SQLite.Simple  -- pkg sqlite-simple
import Text.EditDistance       -- pkg edit-distance

main = do
  gr      <- status "Loading grammar" $ readPGF "Parse.pgf"
  state   <- status "Loading synsets" $ readWordNetAbstract "WordNet.gf"
  state   <- foldM (readWordNetConcrete gr) state
                     (Map.delete "ParseAPI" (languages gr))
  wn30v31 <- status "Loading WordNet 3.0 to 3.1 map" $ readWN30V31Mapping "bootstrap/wn30map31.txt"
  state   <- readOpenMultiWordNet wn30v31 state
  state   <- status "Loading Wikipedia titles" $ readWikipediaTitles "images.txt" "data/titles.txt" state
  state   <- status "Loading PanLex translations" $ readPanLexTranslations "data/panlex.db" state
  transl  <- status "Loading transliteration table" $ readTransliteration "bootstrap/translit.txt"
  state   <- status "Computing levenshtein distances" $ addLevenshteinDistance transl state
  let predictions_fname = "data/predictions.tsv"
  status "Making predictions" $ do    
    let state2 = selectBest state
    writeFile predictions_fname
              (unlines
                 [intercalate "\t" [fn,cnc,lin,show (o,s,w,l,t,c,d)]
                    | (fn,(_,cnc_lins)) <- Map.toList state2
                    , (cnc,lins) <- Map.toList cnc_lins
                    , (lin,o,s,w,l,t,c,d) <- lins
                 ])
  hPutStrLn stderr ("Predictions stored in "++predictions_fname)

readWordNetAbstract fname = do
  fmap (Map.fromList . concatMap parseLine . lines) $
       readFile fname
  where
    parseLine l =
      case words l of
        ("fun":fn:_) -> case break (=='\t') l of
                          (l1,'\t':l2) -> let synset_id = (reverse . take 10 . reverse) l1
                                          in [(fn,(Just synset_id,Map.empty))]
                          _            -> [(fn,(Nothing,Map.empty))]
        _            -> []

readWordNetConcrete gr state cnc =
  status ("Loading linearizations from "++fname) $ do
    fmap (foldl parseLine state . lines) $ readFile fname
  where
    cncName = concreteName cnc
    fname   = "WordNet"++drop 5 cncName++".gf"

    parseLine state l =
      case words l of
        ("lin":fn:ws) -> if not (elem "--guessed" ws) && arity fn == 0 && hasLinearization cnc fn
                           then let s | elem "--unchecked" ws = 1
                                      | otherwise             = 0
                                    lins = [(lin,0,s,1,0,0) | lin <- linearizeAll cnc (mkApp fn [])]
                                in Map.adjust (addLins lins) fn state
                           else state
        _             -> state

    arity fn = maybe 0 (\(hs,_,_) -> length hs) (fmap unType (functionType gr fn))

    addLins lins (mb_synset_id,cnc_lins) = (mb_synset_id,Map.insert cncName lins cnc_lins)

readWN30V31Mapping fpath = do
  fmap toMapping $ readFile fpath
  where
    toMapping =
      Map.fromList .
      map (toTuple . tsv) .
      dropWhile (\l -> take 1 l == "#") .
      lines

    toTuple [tag,wn30,wn31] = (wn30++"-"++tag,wn31++"-"++tag)

readOpenMultiWordNet wn30v31 state = do
  (state,wordnet) <- foldM readLanguage (state,Map.empty) lang_list
  state <- status "Computing co-occurrence counts" $ 
             addCoOccurrenceCount wordnet state
  Map.size state `seq` return state
  where
    readLanguage state_wordnet (_,iso,cnc,_) = do
      let fname = "data/wn-data-"++iso++".tab"
      res <- doesFileExist fname
      if res
        then status ("Loading "++fname) $ readWordNetFile iso cnc fname state_wordnet
        else return state_wordnet

    readWordNetFile iso cnc fname (state,wordnet) = do
      synsets <- fmap (parseLine . tail . lines)
                      (readFile fname)
      return ( fmap (addSynonyms synsets) state
             , Map.insert cnc synsets wordnet
             )
      where
        parseLine =
          foldr addElem Map.empty .
          mapMaybe (toEntry . tsv)
          where
            toEntry [sense_id,rel,w]
              | rel == "lemma" || rel == iso++":lemma" = fmap (flip (,) w) (mapSenseId sense_id)
            toEntry _                                  = Nothing

            mapSenseId sense_id
              | elem cnc wn31langs = Just sense_id
              | otherwise          = Map.lookup sense_id wn30v31

            addElem (k,a) =
              Map.alter (\mb_as -> Just (a:fromMaybe [] mb_as)) k

        addSynonyms synsets x@(Just synset_id,cnc_lins) =
          case Map.lookup synset_id synsets of
            Nothing     -> x
            Just synset -> let cnc_lins' = Map.alter (\mb_lins -> Just (foldl insertInList (fromMaybe [] mb_lins) synset)) cnc cnc_lins
                           in (Just synset_id,cnc_lins')
        addSynonyms synsets x                           = x

        insertInList []                     lemma = [(lemma,1,1,1,0,0)]
        insertInList ((lin,o,s,w,l,t):lins) lemma
          | strMatch lemma lin = (lin,o,s,w,l,t):lins
          | otherwise          = (lin,o,s,w,l,t):insertInList lins lemma

addCoOccurrenceCount wordnet state = do
  return $! Map.mapWithKey update state
  where
    rev_index = Map.mapWithKey reverse wordnet
      where
        reverse cnc synsets =
          Map.fromListWith (++)
                 [(lin,[synset_id])
                      | (synset_id,lins) <- Map.toList synsets
                      , lin <- lins
                      ]

    update fn (mb_sense_id,cnc_lins) =
      let !cnc_lins' = Map.mapWithKey counts cnc_lins
      in (mb_sense_id,cnc_lins')
      where
        counts cnc lins =
          let lins' = map count lins
              force = sum [c | (lin,o,s,w,l,t,c) <- lins']
          in force `seq` lins'
          where
            count (lin,o,s,w,l,t) =
              let synset_ids = fromMaybe [] (Map.lookup cnc rev_index >>= Map.lookup lin)

                  occ =
                    Map.fromListWith (+)
                       [((cnc',lin'),1)
                            | (cnc',synsets) <- Map.toList wordnet
                            , cnc' /= cnc
                            , synset_id <- synset_ids
                            , lin' <- fromMaybe [] (Map.lookup synset_id synsets)
                            ]

                  !c = sum [fromMaybe 0 (Map.lookup (cnc',lin') occ)
                               | (cnc',lins') <- Map.toList cnc_lins
                               , (lin',_,_,_,_,_) <- lins'
                               ]
              in (lin,o,s,w,l,t,c)

readWikipediaTitles img_fname titles_fname state = do
  imgs <- fmap (Map.fromListWith (++) . concatMap parseImageLine . lines)
               (readFile img_fname)
  state <- fmap (parseTitles imgs state . lines)
                (readFile titles_fname)
  return state
  where
    parseImageLine l =
      let fn:refs = tsv l
      in [(page_id,[fn]) | page_id <- map (read . head . cosv) refs :: [Int]]

    parseTitles imgs state (header:ls) =
      let cncs = map (\iso -> head [cnc | (iso',_,cnc,panlex) <- lang_list, iso==iso'])
                      (tail (tsv header))
      in foldl' (parseTitleLine imgs cncs) state ls

    parseTitleLine imgs cncs state l =
      case tsv l of
        (s_id:titles) -> case Map.lookup (read s_id) imgs of
                           Just  fns -> let cnc_titles = [(cnc,title) | (cnc,title) <- zip cncs titles, not (null title)]
                                        in foldl' (updateFun cnc_titles) state fns
                           Nothing   -> state
      where
        updateFun cnc_titles state fn =
          Map.adjust addTitles fn state
          where
            addTitles (mb_synset_id,cnc_lins) =
              (mb_synset_id,foldl addTitle cnc_lins cnc_titles)

            addTitle cnc_lins (cnc,title) = 
              let lins = fromMaybe [] (Map.lookup cnc cnc_lins)
              in Map.insert cnc (insertInList title lins) cnc_lins

            insertInList title []  = [(title,1,2,0,0,0,0)]
            insertInList title ((lin,o,s,w,l,t,c):lins)
              | strMatch title lin = (lin,o,s,0,l,t,c):lins
              | otherwise          = (lin,o,s,w,l,t,c):insertInList title lins

readPanLexTranslations fpath state = do
  conn <- open fpath
  state <- mapM (enrich conn) state
  close conn
  return state
  where
    enrich conn (mb_synset_id,cnc_lins) = do
      ids <- (fmap concat . sequence)
                 [getId lang_var lin | (cnc,lins) <- Map.toList cnc_lins
                                     , (_,_,cnc',lang_var) <- lang_list, cnc==cnc'
                                     , (lin,_,0,_,_,_,_) <- lins]
      new_lins <- fmap (Map.toList . rank . concat) (mapM retrive ids)
      let !cnc_lins' = foldl' (extend new_lins) cnc_lins lang_list
      return (mb_synset_id,cnc_lins')
      where
        getId langvar lin = do
          res <- query conn "select id from expr where langvar=? and txt=?" (langvar,lin) :: IO [Only Int]
          return [id | Only id <- res]

        retrive id = do
          xs <- query conn "select e.langvar,e.txt \
                           \from denotation d1 \
                           \join denotation d2 on d1.expr=? and d1.meaning=d2.meaning \
                           \join meaning m on m.id=d1.meaning and m.source not in (364,469,609,975,1884,2594,2840,2841,3324,3326,3335,3340,3397,3419,3497,3933,4620,5608,5609,6524,6743,7048,3991)\
                           \join expr as e on e.id=d2.expr" (Only id) :: IO [(Int,String)]
          return [(x,[id]) | x <- xs]

        rank = Map.mapWithKey counts . Map.fromListWith (++)
          where
            counts (_,expr) ids = (lengthNub ids,length ids)

            lengthNub = Set.size . Set.fromList

        extend new_lins cnc_lins (_,_,cnc,lang_var) =
          let lins    = fromMaybe [] (Map.lookup cnc cnc_lins)
              transls = [(expr,l,t) | ((lang_var',expr),(l,t)) <- new_lins
                                      , lang_var == lang_var'
                                      ]
              lins'   = foldl' insertInList lins transls
          in Map.insert cnc lins' cnc_lins

        insertInList []                       (transl,l',t') = [(transl,1,2,1,l',t',0)]
        insertInList ((lin,1,2,1,l,t,0):lins) (transl,l',t')
          | l <  l'                                          = [(transl,1,2,1,l',t',0)]
          | l == l'                                          = (transl,1,2,1,l',t',0):(lin,1,2,1,l,t,0):lins
          | otherwise                                        = (lin   ,1,2,1,l ,t ,0):lins
        insertInList ((lin,o,s,w,l,t,c):lins) (transl,l',t')
          | strMatch transl lin = (lin,o,s,w,l',t',c):lins
          | otherwise           = (lin,o,s,w,l, t ,c):insertInList lins (transl,l',t')

readTransliteration :: FilePath -> IO (String -> String)
readTransliteration fpath = do
  transl <- fmap parseLine $ readFile fpath
  return (transliterate transl)
  where
    transliterate transl cs =
      concat [fromMaybe [c] (Map.lookup c transl) | c <- cs]

    parseLine =
      addCase .
      Map.fromList .
      map (toTuple . tsv) .
      lines
      where
        toTuple [[c],t] = (c,t)
        toTuple xs = error (show xs)

        addCase transl =
          let transl' = Map.fromList [(c,fromMaybe [c] (Map.lookup (toLower c) transl)) | c <- [minBound..maxBound], c /= toLower c]
          in Map.union transl transl'

addLevenshteinDistance transl state = do
  return $! (Map.mapWithKey update state)
  where
    update fn (mb_sense_id,cnc_lins) =
      let !cnc_lins' = Map.mapWithKey distances cnc_lins
      in (mb_sense_id,cnc_lins')
      where
        distances cnc lins =
          let lins' = map distance lins
              force = sum [d | (lin,o,s,w,l,t,c,d) <- lins']
          in force `seq` lins'
          where
            distance (lin,o,s,w,l,t,c) =
              let !d = minimum
                         (maxBound:
                          [levenshtein cnc lin cnc2 lin2
                             | (cnc2,lins2) <- Map.toList cnc_lins
                             , cnc /= cnc2
                             , (lin2,_,_,_,_,_,_) <- lins2
                             ])
              in (lin,o,s,w,l,t,c,d)

    levenshtein cnc1 lin1 cnc2 lin2
      |    elem cnc1 no_translit_langs
        || elem cnc2 no_translit_langs = maxBound
      |    elem cnc1 ara_langs
        && elem cnc2 ara_langs
                  = levenshteinDistance defaultEditCosts         lin1          lin2
      |    elem cnc1 cyr_langs
        && elem cnc2 cyr_langs
                  = levenshteinDistance defaultEditCosts         lin1          lin2
      | otherwise = levenshteinDistance defaultEditCosts (transl lin1) (transl lin2)


selectBest state = fmap update state
  where
    update (mb_sense_id,cnc_lins) =
      (mb_sense_id,fmap select cnc_lins)

    select lins = take 1 (sortBy order lins)

    order (_,_,s1,w1,l1,t1,c1,d1) (_,_,s2,w2,l2,t2,c2,d2) =
      compare (s1,-c1,-l1,w1,d1) (s2,-c2,-l2,w2,d2)

-- list of all languages with their iso 2 and iso 3 codes,
-- names of concrete syntaxes and PanLex language variety ids.
lang_list =
   [("af","afr","ParseAfr",7)
   ,("am","amh","ParseAmh",21)
   ,("ar","arb","ParseAra",7399)
   ,("bg","bul","ParseBul",93)
   ,("ca","cat","ParseCat",101)
   ,("cs","cze","ParseCze",106)
   ,("da","dan","ParseDan",150)
   ,("de","deu","ParseGer",157)
   ,("el","ell","ParseGre",184)
   ,("en","eng","ParseEng",187)
   ,("es","spa","ParseSpa",666)
   ,("et","est","ParseEst",190)
   ,("eu","eus","ParseEus",194)
   ,("fa","fas","ParsePes",563)
   ,("fi","fin","ParseFin",204)
   ,("fr","fra","ParseFre",211)
   ,("he","heb","ParseHeb",271)
   ,("hi","hin","ParseHin",275)
   ,("hu","hun","ParseHun",283)
   ,("ia","ina","ParseIna",297)
   ,("is","isl","ParseIce",303)
   ,("it","ita","ParseIta",304)
   ,("ja","jpn","ParseJpn",315)
   ,("kl","kal","ParseKal",319)
   ,("ko","kor","ParseKor",357)
   ,("la","lat","ParseLat",382)
   ,("lv","lav","ParseLav",0)
   ,("mn","mon","ParseMon",0)
   ,("mt","mlt","ParseMlt",442)
   ,("ne","nep","ParseNep",0)
   ,("nl","nld","ParseDut",512)
   ,("no","nob","ParseNor",8480)
   ,("pa","pnb","ParsePnb",2042)
   ,("pl","pol","ParsePol",577)
   ,("pt","por","ParsePor",579)
   ,("ro","ron","ParseRon",611)
   ,("ru","rus","ParseRus",620)
   ,("sd","snd","ParseSnd",659)
   ,("sk","slk","ParseSlo",648)
   ,("sl","slv","ParseSlv",649)
   ,("so","som","ParseSom",662)
   ,("sv","swe","ParseSwe",691)
   ,("sw","swa","ParseSwa",0)
   ,("te","tel","ParseTel",706)
   ,("th","tha","ParseTha",712)
   ,("tr","tur","ParseTur",738)
   ,("ur","urd","ParseUrd",757)
   ,("zh","cmn","ParseChi",1627)
   ]

-- languages for which the files for the Open Multilingual WordNet
-- use the WordNet 3.1 offsets
wn31langs =
   ["ParseEst","ParseGer","ParseTur"]

-- list of languages for which the transliteration doesn't work
no_translit_langs =
   ["ParseChi","ParseJpn","ParseAmh","ParseKor"]

-- list of languages using the Arabic script
ara_langs =
   ["ParseAra","ParsePes","ParseUrd"]

-- list of languages using Cyrilic
cyr_langs =
  ["ParseBul","ParseRus","ParseMon"]

status msg fn = do
  hPutStr stderr (msg++" ...")
  hFlush stderr
  res <- fn
  res `seq` do
    hPutStr stderr "\n"
    hFlush stderr
  return res

strMatch s1 s2 = map toLower s1 == map toLower s2

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

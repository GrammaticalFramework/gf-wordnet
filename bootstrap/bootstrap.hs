{-# LANGUAGE OverloadedStrings, BangPatterns #-}

import PGF2
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List(foldl',sortBy,intercalate)
import Data.Maybe(fromMaybe,fromJust,mapMaybe)
import Data.Char(toLower,isSpace)
import System.IO
import System.Directory
import Database.SQLite.Simple  -- pkg sqlite-simple
import Text.EditDistance       -- pkg edit-distance
import Network.HTTP hiding (close)

main = do
  gr      <- status "Loading grammar" $ readPGF "Parse.pgf"
  state   <- status "Loading synsets" $ readWordNetAbstract "WordNet.gf"
  state   <- foldM (readWordNetConcrete gr) state
                     (Map.delete "ParseAPI" (languages gr))
  wn30v31 <- status "Loading WordNet 3.0 to 3.1 map" $ readWN30V31Mapping "bootstrap/wn30map31.txt"
  state   <- readOpenMultiWordNet wn30v31 state
  state   <- status "Loading Wikidata labels" $ readWikidataLabels state
  state   <- status "Loading PanLex translations" $ readPanLexTranslations "data/panlex.db" state
  transl  <- status "Loading transliteration table" $ readTransliteration "bootstrap/translit.txt"
  state   <- status "Computing Levenshtein distances" $ addLevenshteinDistance transl state
  let predictions_fname = "data/predictions.tsv"
  status "Making predictions" $ do    
    let state2 = selectBest state
    writeFile predictions_fname
              (unlines
                 [intercalate "\t" [fn,cnc,lin,show (o,s,w,l,c,d)]
                    | (fn,(_,cnc_lins)) <- Map.toList state2
                    , (cnc,lins) <- Map.toList cnc_lins
                    , (lin,o,s,w,l,c,d) <- lins
                 ])
  hPutStrLn stderr ("Predictions stored in "++predictions_fname)

readWordNetAbstract fname = do
  fmap (Map.fromList . concatMap parseLine . lines) $
       readFile fname
  where
    parseLine l =
      case words l of
        ("fun":fn:_) -> case break (=='\t') l of
                          (l1,'\t':l2) -> let synset_id = (reverse . dropWhile isSpace . take 10 . reverse) l1
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
                                    lins = [(lin,0,s,1,0) | lin <- linearizeAll cnc (mkApp fn [])]
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
      let fname = "data/omw/wn-data-"++iso++".tab"
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

        insertInList []                   lemma = [(lemma,1,1,1,0)]
        insertInList ((lin,o,s,w,l):lins) lemma
          | strMatch lemma lin = (lin,o,s,w,l):lins
          | otherwise          = (lin,o,s,w,l):insertInList lins lemma

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
              force = sum [c | (lin,o,s,w,l,c) <- lins']
          in force `seq` lins'
          where
            count (lin,o,s,w,l) =
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
                               , (lin',_,_,_,_) <- lins'
                               ]
              in (lin,o,s,w,l,c)
  
readWikidataLabels state = do
  let rq = insertHeader HdrAccept "text/tab-separated-values" $
           insertHeader HdrUserAgent "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36" $
           getRequest "https://query.wikidata.org/sparql?query=SELECT%20%3Fsynset%20%3Flabel%20WHERE%20%7B%20%3Fitem%20wdt%3AP8814%20%3Fsynset.%20%3Fitem%20rdfs%3Alabel%20%3Flabel.%20%7D"
  res1 <- fmap (drop 1 . lines . rspBody) $ simpleHTTP rq
  let rq = insertHeader HdrAccept "text/tab-separated-values" $
           insertHeader HdrUserAgent "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36" $
           getRequest "https://query.wikidata.org/sparql?query=SELECT%20%3Fsynset%20%3Flabel%20WHERE%20%7B%20%3Fitem%20wdt%3AP8814%20%3Fsynset.%20%3Fitem%20skos%3AaltLabel%20%3Flabel.%20%7D"
  res2 <- fmap (drop 1 . lines . rspBody) $ simpleHTTP rq
  let labels = parseLabels (res1++res2)
  state' <- mapM (addLabels labels) state
  return state'
  where
    parseLabels ls =
      Map.fromListWith (++) (map parseLabelLine ls)

    parseLabelLine l =
      case tsv l of
        [f1,f2] -> let synset    = init (tail f1)
                       (s,lcode) = splitLang f2
                   in case [lang | (iso2,iso3,lang,_) <- lang_list, lcode == iso2 || lcode == iso3] of
                        [lang] -> (synset,[(lang,s)])
                        []     -> (synset,[]) 
                        res    -> error ("Ambiguous language code "++lcode++" -> "++show res)
        _            -> error ("Can't parse line: "++l)

    splitLang l = match (tail l) ""
      where
        match ('"':'@':lang) s = (reverse (trim s), lang)
        match (c:cs)         s = match cs (c:s)
        
        trim s =
          case break (\c -> elem c ("(,/\\"::String)) s of
            (_,c:cs) -> reverse (dropWhile isSpace cs)
            _        -> s

    addLabels labels x@(Just synset_id,cnc_lins) =
      case Map.lookup synset_id labels of
        Nothing 
          | take 1 synset_id == "Q"  -> do lins <- readQIDLabels synset_id
                                           let cnc_lins' = foldl addLin cnc_lins lins
                                           return (Just synset_id,cnc_lins')
          | otherwise                -> return x
        Just lins -> let cnc_lins' = foldl addLin cnc_lins lins
                     in return (Just synset_id,cnc_lins')
    addLabels synsets x = return x

    readQIDLabels qid = do
      let rq = insertHeader HdrAccept "text/tab-separated-values" $
               insertHeader HdrUserAgent "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36" $
               getRequest ("https://query.wikidata.org/sparql?query=SELECT%20%3Flabel%0AWHERE%20%0A%7B%0A%20%20%7B%20wd%3A"++qid++"%20rdfs%3Alabel%20%3Flabel.%20%7D%20UNION%20%7B%20wd%3A"++qid++"%20skos%3AaltLabel%20%3Flabel.%20%7D%0A%7D")
      res <- fmap (drop 1 . lines . rspBody) $ simpleHTTP rq
      return (concatMap parseLabelLine res)
      where
        parseLabelLine l =
          let (s,lcode) = splitLang l
          in case [lang | (iso2,iso3,lang,_) <- lang_list, lcode == iso2 || lcode == iso3] of
               [lang] -> [(lang,s)]
               []     -> []
               res    -> error ("Ambiguous language code "++lcode++" -> "++show res)

    addLin cnc_lins (cnc,lin) =
      Map.alter (\mb_lins -> Just $ insertInList lin (fromMaybe [] mb_lins)) cnc cnc_lins

    insertInList lin0 []  = [(lin0,1,2,0,0,0)]
    insertInList lin0 ((lin,o,s,w,l,c):lins)
      | strMatch lin0 lin = (lin,o,s,0,l,c):lins
      | otherwise         = (lin,o,s,w,l,c):insertInList lin0 lins


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
                                     , (lin,_,0,_,_,_) <- lins]
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
            counts _ ids = (Set.size . Set.fromList) ids

        extend new_lins cnc_lins (_,_,cnc,lang_var) =
          let lins    = fromMaybe [] (Map.lookup cnc cnc_lins)
              transls = [(expr,l) | ((lang_var',expr),l) <- new_lins
                                  , lang_var == lang_var'
                                  ]
              lins'   = foldl' insertInList lins transls
          in Map.insert cnc lins' cnc_lins

        insertInList []                     (transl,l') = [(transl,1,2,1,l',0)]
        insertInList ((lin,1,2,1,l,0):lins) (transl,l')
          | l <  l'                                     = [(transl,1,2,1,l',0)]
          | l == l'                                     = (transl,1,2,1,l',0):(lin,1,2,1,l,0):lins
          | otherwise                                   = (lin   ,1,2,1,l ,0):lins
        insertInList ((lin,o,s,w,l,c):lins) (transl,l')
          | strMatch transl lin = (lin,o,s,w,l',c):lins
          | otherwise           = (lin,o,s,w,l ,c):insertInList lins (transl,l')

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
              force = sum [d | (lin,o,s,w,l,c,d) <- lins']
          in force `seq` lins'
          where
            distance (lin,o,s,w,l,c) =
              let !d = minimum
                         (maxBound:
                          [levenshtein cnc lin cnc2 lin2
                             | (cnc2,lins2) <- Map.toList cnc_lins
                             , cnc /= cnc2
                             , (lin2,_,_,_,_,_) <- lins2
                             ])
              in (lin,o,s,w,l,c,d)

    levenshtein cnc1 lin1 cnc2 lin2
      |    elem cnc1 no_translit_langs
        || elem cnc2 no_translit_langs = maxBound
      |    elem cnc1 ara_langs
        && elem cnc2 ara_langs
                  = levenshteinDistance defaultEditCosts         lin1          lin2
      |    elem cnc1 cyr_langs
        && elem cnc2 cyr_langs
                  = levenshteinDistance defaultEditCosts         lin1          lin2
      |    elem cnc1 gre_langs
        && elem cnc2 gre_langs
                  = levenshteinDistance defaultEditCosts         lin1          lin2
      |    elem cnc1 heb_langs
        && elem cnc2 heb_langs
                  = levenshteinDistance defaultEditCosts         lin1          lin2
      |    elem cnc1 dev_langs
        && elem cnc2 dev_langs
                  = levenshteinDistance defaultEditCosts         lin1          lin2
      |    elem cnc1 chi_langs
        && elem cnc2 chi_langs
                  = levenshteinDistance defaultEditCosts         lin1          lin2
      | otherwise = levenshteinDistance defaultEditCosts (transl lin1) (transl lin2)


selectBest state = fmap update state
  where
    update (mb_sense_id,cnc_lins) =
      (mb_sense_id,fmap select cnc_lins)

    select lins = take 1 (sortBy order lins)

    order (_,_,s1,w1,l1,c1,d1) (_,_,s2,w2,l2,c2,d2) =
      compare (s1,-c1,-l1,w1,d1) (s2,-c2,-l2,w2,d2)

-- list of all languages with their iso 2 and iso 3 codes,
-- names of concrete syntaxes and PanLex language variety ids.
lang_list =
   [("aa","aar","ParseAar",1)
   ,("ab","abk","ParseAbk",3)
   ,(""  ,"ace","ParseAce",1786)
   ,(""  ,"ady","ParseAdy",6)
   ,("af","afr","ParseAfr",7)
   ,("ak","aka","ParseAka",14)
   ,(""  ,"als","ParseAls",162)
   ,(""  ,"alt","ParseAlt",20)
   ,(""  ,"ami","ParseAmi",302)
   ,(""  ,"ang","ParseAng",23)
   ,("am","amh","ParseAmh",21)
   ,("an","arg","ParseArg",36)
   ,("ar","arb","ParseAra",34)
   ,(""  ,"arc","ParseArc",35)
   ,(""  ,"arq","ParseArq",2089)
   ,(""  ,"ary","ParseAry",44)
   ,(""  ,"arz","ParseArz",45)
   ,("as","asm","ParseAsm",47)
   ,(""  ,"ast","ParseAst",48)
   ,(""  ,"atj","ParseAtj",7756)
   ,("av","ava","ParseAva",50)
   ,(""  ,"avk","ParseAvk",9115)
   ,(""  ,"awa","ParseAwa",52)
   ,("ay","aym","ParseAym",1406)
   ,("az","azj","ParseAzj",54)
   ,(""  ,"azb","ParseAzb",212)
   ,("ba","bak","ParseBak",57)
   ,(""  ,"ban","ParseBan",60)
   ,(""  ,"bar","ParseBar",61)
   ,(""  ,"bcl","ParseBcl",76)
   ,("be","bel","ParseBel",66)
   ,(""  ,"bgn","ParseBgn",1243)
   ,("bg","bul","ParseBul",93)
   ,("bh","bho","ParseBho",75)
   ,("bi","bis","ParseBis",77)
   ,(""  ,"bjn","ParseBjn",322)
   ,(""  ,"blk","ParseBlk",3268)
   ,("bm","bam","ParseBam",59)
   ,("bn","ben","ParseBen",67)
   ,("bo","bod","ParseBod",82)
   ,(""  ,"bpy","ParseBpy",85)
   ,("br","bre","ParseBre",86)
   ,("bs","bos","ParseBos",83)
   ,(""  ,"bug","ParseBug",92)
   ,(""  ,"bxr","ParseBxr",475)
   ,("ca","cat","ParseCat",101)
   ,("ch","cha","ParseCha",108)
   ,(""  ,"cdo","ParseCdo",26)
   ,("ce","che","ParseChe",110)
   ,(""  ,"ceb","ParseCeb",104)
   ,(""  ,"cho","ParseCho",112)
   ,(""  ,"chr","ParseChr",114)
   ,(""  ,"chy","ParseChy",118)
   ,(""  ,"ckb","ParseCkb",122)
   ,("co","cos","ParseCos",133)
   ,("cr","cre","ParseCre",135)
   ,("cs","ces","ParseCze",106)
   ,(""  ,"csb","ParseCsb",141)
   ,(""  ,"crh","ParseCrh",136)
   ,("cu","chu","ParseChu",116)
   ,("cv","chv","ParseChv",117)
   ,("cy","cym","ParseCym",147)
   ,("da","dan","ParseDan",150)
   ,(""  ,"dag","ParseDag",2534)
   ,("de","deu","ParseGer",157)
   ,(""  ,"din","ParseDin",160)
   ,(""  ,"diq","ParseDiq",514)
   ,(""  ,"dsb","ParseDsb",170)
   ,(""  ,"dty","ParseDty",11787)
   ,("dv","div","ParseDiv",161)
   ,("dz","dzo","ParseDzo",179)
   ,("ee","ewe","ParseEwe",197)
   ,("el","ell","ParseGre",184)
   ,(""  ,"eml","ParseEml",1619)
   ,("en","eng","ParseEng",187)
   ,("eo","epo","ParseEpo",189)
   ,("es","spa","ParseSpa",666)
   ,("et","est","ParseEst",190)
   ,("eu","eus","ParseEus",194)
   ,(""  ,"ext","ParseExt",68)
   ,("fa","fas","ParsePes",563)
   ,("ff","ful","ParseFul",222)
   ,("fi","fin","ParseFin",204)
   ,("fj","fij","ParseFij",202)
   ,("fo","fao","ParseFao",200)
   ,("fr","fra","ParseFre",211)
   ,(""  ,"frc","ParseFrc",1257)
   ,(""  ,"frp","ParseFrp",217)
   ,(""  ,"frr","ParseFrr",218)
   ,("fy","fry","ParseFry",219)
   ,(""  ,"fur","ParseFur",225)
   ,("ga","gle","ParseGle",238)
   ,(""  ,"gag","ParseGag",229)
   ,(""  ,"gan","ParseGan",231)
   ,(""  ,"gcr","ParseGcr",580)
   ,("gd","gla","ParseGla",236)
   ,(""  ,"gom","ParseGom",1837)
   ,(""  ,"gor","ParseGor",607)
   ,(""  ,"got","ParseGot",250)
   ,("gl","glg","ParseGlg",239)
   ,(""  ,"glk","ParseGlk",242)
   ,("",  "grc","ParseGrc",251)
   ,("gn","grn","ParseGrn",7457)
   ,(""  ,"gsw","ParseGsw",253)
   ,("gu","guj","ParseGuj",254)
   ,("",  "gur","ParseGur",1516)
   ,(""  ,"guw","ParseGuw",12384)
   ,("gv","glv","ParseGlv",243)
   ,("ha","hau","ParseHau",267)
   ,(""  ,"hak","ParseHak",263)
   ,(""  ,"haw","ParseHaw",268)
   ,("he","heb","ParseHeb",271)
   ,("hi","hin","ParseHin",275)
   ,(""  ,"hif","ParseHif",1613)
   ,("ho","hmo","ParseHmo",5283)
   ,("hr","hrv","ParseHrv",280)
   ,(""  ,"hsb","ParseHsb",281)
   ,("ht","hat","ParseHat",266)
   ,("hu","hun","ParseHun",283)
   ,("hy","hye","ParseHye",285)
   ,(""  ,"hyw","ParseHyw",1486)
   ,("ia","ina","ParseIna",297)
   ,("id","ind","ParseInd",298)
   ,("ie","ile","ParseIle",293)
   ,("ig","ibo","ParseIbo",288)
   ,("ii","iii","ParseIii",290)
   ,("ik","ipk","ParseIpk",11226)
   ,(""  ,"ilo","ParseIlo",295)
   ,(""  ,"inh","ParseInh",300)
   ,("is","isl","ParseIce",303)
   ,("it","ita","ParseIta",304)
   ,("io","ido","ParseIdo",289)
   ,("iu","iku","ParseIku",291)
   ,("ja","jpn","ParseJpn",315)
   ,(""  ,"jam","ParseJam",307)
   ,(""  ,"jbo","ParseJbo",310)
   ,("jv","jav","ParseJav",309)
   ,("ka","kat","ParseKat",324)
   ,(""  ,"kaa","ParseKaa",316)
   ,(""  ,"kab","ParseKab",318)
   ,(""  ,"kbd","ParseKbd",327)
   ,(""  ,"kbp","ParseKbp",2555)
   ,(""  ,"kcg","ParseKcg",2778)
   ,("kg","kon","ParseKon",356)
   ,("ki","kik","ParseKik",339)
   ,("kk","kaz","ParseKaz",326)
   ,("kl","kal","ParseKal",319)
   ,("km","khm","ParseKhm",336)
   ,("kn","kan","ParseKan",320)
   ,("ko","kor","ParseKor",357)
   ,("kr","kau","ParseKau",2374)
   ,(""  ,"krc","ParseKrc",359)
   ,("ks","kas","ParseKas",323)
   ,(""  ,"ksh","ParseKsh",363)
   ,(""  ,"koi","ParseKoi",353)
   ,("ku","kur","ParseKur",371)
   ,("kv","kpv","ParseKpv",355)
   ,("kw","cor","ParseCor",131)
   ,("ky","kir","ParseKir",342)
   ,("la","lat","ParseLat",382)
   ,(""  ,"lad","ParseLad",375)
   ,("lb","ltz","ParseLtz",399)
   ,(""  ,"lbe","ParseLbe",384)
   ,(""  ,"lez","ParseLez",386)
   ,(""  ,"lfn","ParseLfn",5609)
   ,("lg","lug","ParseLug",401)
   ,("li","lim","ParseLim",389)
   ,(""  ,"lij","ParseLij",387)
   ,(""  ,"lld","ParseLld",395)
   ,(""  ,"lmo","ParseLmo",396)
   ,("ln","lin","ParseLin",390)
   ,("lo","lao","ParseLao",381)
   ,(""  ,"lrc","ParseLrc",2022)
   ,("lt","lit","ParseLit",391)
   ,(""  ,"ltg","ParseLtg",1946)
   ,("lv","lav","ParseLav",383)
   ,("",  "lzh","ParseLzh",1263)
   ,("",  "lzz","ParseLzz",404)
   ,(""  ,"mai","ParseMai",408)
   ,(""  ,"mad","ParseMad",405)
   ,(""  ,"mcn","ParseMcn",1717)
   ,(""  ,"mdf","ParseMdf",414)
   ,("mg","mlg","ParseMlg",440)
   ,("mh","mah","ParseMah",406)
   ,(""  ,"mhr","ParseMhr",1570)
   ,("mi","mri","ParseMao",461)
   ,(""  ,"min","ParseMin",433)
   ,("mk","mkd","ParseMkd",439)
   ,("ml","mal","ParseMal",409)
   ,("ms","zsm","ParseZsm",444)
   ,("mn","mon","ParseMon",454)
   ,(""  ,"mni","ParseMni",447)
   ,(""  ,"mnw","ParseMnw",450)
   ,("mr","mar","ParseMar",411)
   ,(""  ,"mrj","ParseMrj",463)
   ,("mt","mlt","ParseMlt",442)
   ,(""  ,"mus","ParseMus",468)
   ,(""  ,"mwl","ParseMwl",470)
   ,("my","mya","ParseMya",473)
   ,(""  ,"myv","ParseMyv",479)
   ,(""  ,"mzn","ParseMzn",71)
   ,(""  ,"nan","ParseNan",483)
   ,("na","nau","ParseNau",491)
   ,(""  ,"nah","ParseNah",11843)
   ,(""  ,"nap","ParseNap",487)
   ,(""  ,"nds","ParseNds",497)
   ,("ne","nep","ParseNep",503)
   ,(""  ,"new","ParseNew",505)
   ,("ng","ndo","ParseNdo",496)
   ,(""  ,"nia","ParseNia",919)
   ,("nl","nld","ParseDut",512)
   ,("nn","nno","ParseNno",517)
   ,("nb","nob","ParseNor",924)
   ,(""  ,"nov","ParseNov",523)
   ,(""  ,"nrm","ParseNrm",6929)
   ,(""  ,"nso","ParseNso",526)
   ,(""  ,"nqo","ParseNqo",9750)
   ,("nv","nav","ParseNav",492)
   ,("ny","nya","ParseNya",528)
   ,("oc","oci","ParseOci",533)
   ,(""  ,"olo","ParseOlo",3009)
   ,("om","orm","ParseOrm",544)
   ,("or","ori","ParseOri",8504)
   ,("os","oss","ParseOss",545)
   ,("pa","pnb","ParsePnb",2042)
   ,(""  ,"pap","ParsePap",555)
   ,(""  ,"pag","ParsePag",550)
   ,(""  ,"pam","ParsePam",552)
   ,(""  ,"pcd","ParsePcd",560)
   ,(""  ,"pcm","ParsePcm",1585)
   ,(""  ,"pdc","ParsePdc",561)
   ,(""  ,"pfl","ParsePfl",4007)
   ,("pi","pli","ParsePli",452)
   ,(""  ,"pih","ParsePih",565)
   ,("pl","pol","ParsePol",577)
   ,(""  ,"pms","ParsePms",574)
   ,(""  ,"pnt","ParsePnt",9119)
   ,("ps","pus","ParsePus",1644)
   ,("pt","por","ParsePor",579)
   ,(""  ,"prg","ParsePrg",583)
   ,(""  ,"pwn","ParsePwn",592)
   ,("qu","que","ParseQue",593)
   ,(""  ,"rgn","ParseRgn",1619) 
   ,(""  ,"rmy","ParseRmy",601)
   ,("rm","roh","ParseRoh",608)
   ,("ro","ron","ParseRon",611)
   ,("rn","run","ParseRun",615)
   ,("",  "rup","ParseRup",618)
   ,("ru","rus","ParseRus",620)
   ,(""  ,"rue","ParseRue",613)
   ,("rw","kin","ParseKin",341)
   ,("sa","san","ParseSan",624)
   ,(""  ,"sah","ParseSah",623)
   ,(""  ,"sat","ParseSat",626)
   ,("sc","srd","ParseSrd",5477)
   ,(""  ,"sco","ParseSco",630)
   ,(""  ,"scn","ParseScn",629)
   ,(""  ,"sgs","ParseSgs",1259)
   ,("sd","snd","ParseSnd",659)
   ,(""  ,"sma","ParseSma",650)
   ,("se","sme","ParseSme",651)
   ,("",  "smj","ParseSmj",654)
   ,(""  ,"sms","ParseSms",657)
   ,("sg","sag","ParseSag",621)
   ,(""  ,"shi","ParseShi",636)
   ,(""  ,"shn","ParseShn",3106)
   ,("si","sin","ParseSin",640)
   ,("sk","slk","ParseSlo",648)
   ,(""  ,"skr","ParseSkr",644)
   ,("sl","slv","ParseSlv",649)
   ,("sm","smo","ParseSmo",656)
   ,(""  ,"smn","ParseSmn",655)
   ,("sn","sna","ParseSna",658)
   ,("so","som","ParseSom",662)
   ,("sq","sqi","ParseSqi",5487)
   ,("sr","srp","ParseSrp",676)
   ,(""  ,"srn","ParseSrn",674)
   ,("ss","ssw","ParseSsw",678)
   ,("st","sot","ParseSot",664)
   ,(""  ,"stq","ParseStq",679)
   ,("su","sun","ParseSun",685)
   ,("sv","swe","ParseSwe",691)
   ,("sw","swa","ParseSwa",6665)
   ,(""  ,"szl","ParseSzl",142)
   ,(""  ,"szy","ParseSzy",13035)
   ,("ta","tam","ParseTam",698)
   ,(""  ,"tay","ParseTay",988)
   ,(""  ,"tcy","ParseTcy",704)
   ,("te","tel","ParseTel",706)
   ,(""  ,"tet","ParseTet",708)
   ,("tg","tgk","ParseTgk",710)
   ,("th","tha","ParseTha",712)
   ,("ti","tir","ParseTir",715)
   ,("tk","tuk","ParseTuk",737)
   ,("tl","tgl","ParseTgl",711)
   ,("tn","tsn","ParseTsn",731)
   ,("to","ton","ParseTon",723)
   ,(""  ,"tpi","ParseTpi",724)
   ,("tr","tur","ParseTur",738)
   ,(""  ,"trv","ParseTrv",728)
   ,("ts","tso","ParseTso",733)
   ,("tt","tat","ParseTat",701)
   ,(""  ,"tum","ParseTum",1007)
   ,("tw","twi","ParseTwi",742)
   ,("ty","tah","ParseTah",697)
   ,(""  ,"tyv","ParseTyv",745)
   ,(""  ,"udm","ParseUdm",752)
   ,("ug","uig","ParseUig",754)
   ,("uk","ukr","ParseUkr",755)
   ,("ur","urd","ParseUrd",757)
   ,("uz","uzb","ParseUzb",759)
   ,(""  ,"vec","ParseVec",760)
   ,(""  ,"vep","ParseVep",763)
   ,("vi","vie","ParseVie",764)
   ,(""  ,"vls","ParseVls",765)
   ,("ve","ven","ParseVen",762)
   ,(""  ,"vmf","ParseVmf",1022)
   ,("vo","vol","ParseVol",769)
   ,("",  "vro","ParseVro",1260)
   ,("wa","wln","ParseWln",785)
   ,(""  ,"war","ParseWar",1026)
   ,("wo","wol","ParseWol",787)
   ,(""  ,"wuu","ParseWuu",790)
   ,(""  ,"xal","ParseXal",793)
   ,("xh","xho","ParseXho",798)
   ,(""  ,"xmf","ParseXmf",802)
   ,("yi","yid","ParseYid",811)
   ,("yo","yor","ParseYor",814)
   ,("",  "yue","ParseYue",820)
   ,("za","zha","ParseZha",11783)
   ,(""  ,"zea","ParseZea",78)
   ,("zh","cmn","ParseChi",1627)
   ,("zu","zul","ParseZul",832)
   ]

-- languages for which the files for the Open Multilingual WordNet
-- use the WordNet 3.1 offsets
wn31langs =
   ["ParseEst","ParseGer","ParseTur"]

-- list of languages for which the transliteration doesn't work
no_translit_langs :: [ConcName]
no_translit_langs =
   ["ParseAmh","ParseBgn","ParseBod","ParseChi","ParseChr","ParseDzo",
    "ParseGot","ParseGuj","ParseHye","ParseHyw","ParseIii","ParseIku",
    "ParseJpn","ParseKat","ParseKor","ParseKhm","ParseMal","ParseMnw",
    "ParseMya","ParseNqo","ParseOri","ParseSat","ParseShn","ParseSin",
    "ParseTam","ParseTcy","ParseTel","ParseTir","ParseXmf","ParseYor",
    "ParseZha"]

-- list of languages using the Arabic script
ara_langs :: [ConcName]
ara_langs =
   ["ParseAra","ParseArz","ParseAzb","ParseGlk","ParseKas","ParseKur",
    "ParseLrc","ParseMzn","ParsePes","ParsePnb","ParsePus","ParseSnd",
    "ParseUig","ParseUrd"]

-- list of languages using Cyrilic
cyr_langs :: [ConcName]
cyr_langs =
  ["ParseAbk","ParseAdy","ParseAlt","ParseAva","ParseBak","ParseBel",
   "ParseBul","ParseBxr","ParseChe","ParseChu","ParseChv","ParseInh",
   "ParseKaz","ParseKir","ParseKoi","ParseKpv","ParseKrc","ParseLbe",
   "ParseLez","ParseMdf","ParseMhr","ParseMkd","ParseMlg","ParseMon",
   "ParseMrj","ParseMyv","ParseOss","ParseRue","ParseRus","ParseSah",
   "ParseSrp","ParseTat","ParseTgk","ParseTyv","ParseUdm","ParseUkr",
   "ParseXal"]
   
heb_langs :: [ConcName]
heb_langs = 
  ["ParseHeb","ParseYid"]

gre_langs :: [ConcName]
gre_langs = 
  ["ParseGrc","ParseGre","ParsePnt"]
  
dev_langs :: [ConcName]
dev_langs =
  ["ParseBen","ParseBho","ParseBpy","ParseGom","ParseHin","ParseMar"]

chi_langs :: [ConcName]
chi_langs =
  ["ParseChi","ParseGan","ParseHak","ParseNan","ParseNep","ParseNew",
   "ParseSan","ParseWuu","ParseYue"]


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

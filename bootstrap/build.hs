import PGF2
import Data.Char(toLower)
import Data.Maybe(fromMaybe)
import Data.List(intercalate)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import System.IO
import System.Directory
import System.Environment
import System.FilePath(takeBaseName)
import System.Process

main = do
  [fname] <- getArgs
  let lang = drop 7 (takeBaseName fname)
      cnc  = "Parse" ++ lang
  state <- readPredictions "data/predictions.tsv"
  draft <- readDraft fname lang
  morphoMap <- readMorpho lang
  writeFile fname (unlines (applyChanges morphoMap (fromMaybe Map.empty (Map.lookup cnc state)) draft))

readPredictions fname =
  fmap (Map.fromListWith (Map.unionWith (++)) . map parseLine . lines)
       (readFile fname)
  where
    parseLine line = (cnc,Map.singleton fn [(lin,o,s,w,l,c,d)])
      where
        [fn,cnc,lin,str] = tsv line
        (o,s,w,l,c,d)  = read str :: (Int,Int,Int,Int,Int,Int)

readDraft fname lang = do
  exists <- doesFileExist fname
  if exists
    then do ls <- fmap lines
                       (readFile fname)
            length ls `seq` return ls
    else fmap (mkDraft . lines)
              (readFile "WordNet.gf")
  where
    mkDraft ls =
      ["concrete "++takeBaseName fname++" of WordNet = Cat"++lang++" ** open Paradigms"++lang++", Grammar"++lang++", (S=Structural"++lang++") in {"
      ,""
      ] ++
      [l | l <- ls, l <- convert l] ++
      [""
      ,"}"
      ]
      where
         convert l =
           case words l of
             ("fun":fn:_) | Set.member fn structuralSet -> ["lin "++fn++" = S."++fn++" ;"]
                          | otherwise                   -> ["lin "++fn++" = variants {} ;"]
             _                                          -> []

applyChanges morphoMap patches draft = [patch l | l <- draft]
  where
    patch l =
      case words l of
        ("lin":fn:_) -> case Map.lookup fn patches of
                          Nothing                                         -> l
                          Just lins | all (\(_,o,_,_,_,_,_) -> o==0) lins -> l
                                    | otherwise                           -> "lin "++fn++" = "++prediction2gf morphoMap fn lins
        _            -> l

readMorpho lang =
  fmap toMorphoEntries $ return "" -- readCreateProcess (shell ("cat lib/src/"++dir++"*/Dict???.gf lib/src/"++dir++"*/Irreg???.gf")) ""
  where
    dir = map toLower lang

    toMorphoEntries = 
      Map.fromList .
      concatMap parseLine .
      lines
      where
        parseLine l =
          case words l of
            ("lin":fn:"=":ws) -> [(unpack fn,unwords (init ws))]
            _                 -> []

    unpack fn
      | take 1 fn == "'" && take 1 (reverse fn) == "'" = init (tail fn)
      | otherwise                                      = fn

prediction2gf morphoMap fn lins =
  case lins of
    [lin] -> mkBody lin ++ " ;"++status
    _     -> "variants {"++intercalate "; " (map mkBody lins)++"} ;"++status
  where
    status =
      case head lins of
        (lin,o,s,w,l,c,d) | s == 0          -> ""
                          | s == 1 && l > 1 -> ""
                          | w == 0 && l > 1 -> ""
                          | w == 0 && s == 1-> ""
                          | s == 1          -> " --unchecked"
                          | otherwise       -> " --guessed"

    (abs,cat) = splitOnElemRight '_' fn
    mkBody (lin,o,s,w,l,c,d) =
      Map.findWithDefault (\_ _ _ -> "variants {}") cat functionMap morphoMap fn lin

functionMap :: Map.Map Cat (Map.Map Fun String -> Fun -> String -> String)
functionMap = Map.fromList [
  -- missing Card and Predet because too complicated
  ("A"      , \morphoMap fun lemma -> look morphoMap fun (lo lemma) "A"),
  ("A2"     , \morphoMap fun lemma -> "mkA2 ("++look morphoMap fun (lo lemma) "A"++") noPrep"),
  ("AdA"    , \morphoMap fun lemma -> look morphoMap fun (lo lemma) "AdA"),
  ("AdN"    , \morphoMap fun lemma -> look morphoMap fun (lo lemma) "AdN"),
  ("AdV"    , \morphoMap fun lemma -> look morphoMap fun (lo lemma) "AdV"),
  ("Adv"    , \morphoMap fun lemma -> look morphoMap fun (lo lemma) "Adv"),
  ("CN"     , \morphoMap fun lemma -> "UseN ("++look morphoMap fun (lo lemma) "N"++")"),
  ("Interj" , \morphoMap fun lemma -> "ss \""++(lo lemma)++"\""),
  ("N"      , \morphoMap fun lemma -> look morphoMap fun (lo lemma) "N"),
  ("N2"     , \morphoMap fun lemma -> "mkN2 ("++look morphoMap fun (lo lemma) "N"++") noPrep"),
  ("PN"     , \morphoMap fun lemma -> look morphoMap fun lemma "PN"),
  ("Prep"   , \morphoMap fun lemma -> look morphoMap fun (lo lemma) "Prep"),
  ("V"      , \morphoMap fun lemma -> look morphoMap fun (lo lemma) "V"),
  ("V2"     , \morphoMap fun lemma -> "dirV2 ("++look morphoMap fun (lo lemma) "V"++")"),
  ("V2A"    , \morphoMap fun lemma -> "mkV2A ("++look morphoMap fun (lo lemma) "V"++") noPrep noPrep"),
  ("V2S"    , \morphoMap fun lemma -> "mkV2S ("++look morphoMap fun (lo lemma) "V"++") noPrep"),
  ("V2V"    , \morphoMap fun lemma -> "mkV2V ("++look morphoMap fun (lo lemma) "V"++") noPrep noPrep"),
  ("V3"     , \morphoMap fun lemma -> "mkV3 ("++look morphoMap fun (lo lemma) "V"++") noPrep noPrep"),
  ("VA"     , \morphoMap fun lemma -> "mkVA ("++look morphoMap fun (lo lemma) "V"++")"),
  ("VQ"     , \morphoMap fun lemma -> "mkVQ ("++look morphoMap fun (lo lemma) "V"++")"),
  ("VS"     , \morphoMap fun lemma -> "mkVS ("++look morphoMap fun (lo lemma) "V"++")"),
  ("VV"     , \morphoMap fun lemma -> "mkVV ("++look morphoMap fun (lo lemma) "V"++")"),
  ("Voc"    , \morphoMap fun lemma -> "ss \""++(lo lemma)++"\"")
  ]
  where
    lo = map toLower

    look morphoMap fun lemma cat
      | contains "Masc_" fun = Map.findWithDefault ("mk"++cat++" \""++lemma++"\"") (lemma'++"Masc_"++cat) morphoMap
      | contains "Fem_"  fun = Map.findWithDefault ("mk"++cat++" \""++lemma++"\"") (lemma'++"Fem_"++cat) morphoMap
      | otherwise            = Map.findWithDefault ("mk"++cat++" \""++lemma++"\"") (lemma'++"_"++cat) morphoMap
      where
        lemma' = map toLower lemma

structuralSet = Set.fromList [
  "above_Prep",
  "after_Prep",
  "all_Predet",
  "almost_AdA",
  "almost_AdN",
  "although_Subj",
  "always_AdV",
  "and_Conj",
  "because_Subj",
  "before_Prep",
  "behind_Prep",
  "between_Prep",
  "both7and_DConj",
  "but_PConj",
  "can_VV",
  "during_Prep",
  "either7or_DConj",
  "every_Det",
  "everywhere_Adv",
  "few_Det",
  "for_Prep",
  "from_Prep",
  "he_Pron",
  "here_Adv",
  "how_IAdv",
  "i_Pron",
  "if_Subj",
  "in_Prep",
  "it_Pron",
  "less_CAdv",
  "many_Det",
  "more_CAdv",
  "most_Predet",
  "much_Det",
  "must_VV",
  "on_Prep",
  "only_Predet",
  "or_Conj",
  "otherwise_PConj",
  "part_Prep",
  "please_Voc",
  "possess_Prep",
  "quite_Adv",
  "she_Pron",
  "so_AdA",
  "somewhere_Adv",
  "that_Quant",
  "that_Subj",
  "there_Adv",
  "therefore_PConj",
  "they_Pron",
  "this_Quant",
  "through_Prep",
  "to_Prep",
  "too_AdA",
  "under_Prep",
  "very_AdA",
  "want_VV",
  "we_Pron",
  "whatPl_IP",
  "whatSg_IP",
  "when_IAdv",
  "when_Subj",
  "where_IAdv",
  "which_IQuant",
  "whoPl_IP",
  "whoSg_IP",
  "why_IAdv",
  "with_Prep",
  "without_Prep",
  "youSg_Pron",
  "youPl_Pron",
  "youPol_Pron",
  "no_Quant",
  "not_Predet",
  "at_least_AdN",
  "at_most_AdN",
  "except_Prep",
  "as_CAdv"
  ]

splitOnElemRight :: Eq a => a -> [a] -> ([a],[a])
splitOnElemRight e = split [] . reverse
  where
    split xs [] = (xs, [])
    split xs (z:zt) = if z == e
                      then (reverse zt, xs)
                      else split (z:xs) zt

contains s1 []                = False 
contains s1 s2
  | take (length s1) s2 == s1 = True
contains s1 (_:s2)            = contains s1 s2

tsv :: String -> [String]
tsv "" = []
tsv cs =
  let (x,cs1) = break (=='\t') cs
  in x : if null cs1 then [] else tsv (tail cs1)

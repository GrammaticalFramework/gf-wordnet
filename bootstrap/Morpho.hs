module Morpho(readMorpho, prediction2gf) where

import PGF2
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List
import System.Process

readMorpho lang =
  fmap toMorphoEntries $ readCreateProcess (shell ("cat lib/src/"++lang++"*/Dict???.gf lib/src/"++lang++"*/Irreg???.gf")) ""

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

prediction2gf :: String -> Map.Map Fun String -> Map.Map Fun [String] -> Fun -> String
prediction2gf tag morphoMap dict lex_id
  | Set.member lex_id structuralSet = "S."++lex_id++" ;"
  | otherwise                       =
      case Map.lookup lex_id dict of
        Just forms -> case forms of
                        [f] -> mkBody f ++ " ; --"++tag
                        _   -> "variants {"++intercalate "; " (map mkBody forms)++"} ; --"++tag
        Nothing    -> "variants {} ;"
  where
    (abs,cat) = splitOnElemRight '_' lex_id
    mkBody    = Map.findWithDefault (\_ _ _ -> "variants {}") cat functionMap morphoMap lex_id

functionMap :: Map.Map Cat (Map.Map Fun String -> Fun -> String -> String)
functionMap = Map.fromList [
  -- missing Card and Predet because too complicated
  ("A"      , \morphoMap fun lemma -> look morphoMap fun lemma "A"),
  ("A2"     , \morphoMap fun lemma -> "mkA2 ("++look morphoMap fun lemma "A"++") noPrep"),
  ("AdA"    , \morphoMap fun lemma -> look morphoMap fun lemma "AdA"),
  ("AdN"    , \morphoMap fun lemma -> look morphoMap fun lemma "AdN"),
  ("AdV"    , \morphoMap fun lemma -> look morphoMap fun lemma "AdV"),
  ("Adv"    , \morphoMap fun lemma -> look morphoMap fun lemma "Adv"),
  ("CN"     , \morphoMap fun lemma -> "UseN ("++look morphoMap fun lemma "N"++")"),
  ("Interj" , \morphoMap fun lemma -> "ss \""++lemma++"\""),
  ("N"      , \morphoMap fun lemma -> look morphoMap fun lemma "N"),
  ("N2"     , \morphoMap fun lemma -> "mkN2 ("++look morphoMap fun lemma "N"++") noPrep"),
  ("PN"     , \morphoMap fun lemma -> look morphoMap fun lemma "PN"),
  ("Prep"   , \morphoMap fun lemma -> look morphoMap fun lemma "Prep"),
  ("V"      , \morphoMap fun lemma -> look morphoMap fun lemma "V"),
  ("V2"     , \morphoMap fun lemma -> "mkV2 ("++look morphoMap fun lemma "V"++")"),
  ("V2A"    , \morphoMap fun lemma -> "mkV2A ("++look morphoMap fun lemma "V"++") noPrep noPrep"),
  ("V2S"    , \morphoMap fun lemma -> "mkV2S ("++look morphoMap fun lemma "V"++") noPrep"),
  ("V2V"    , \morphoMap fun lemma -> "mkV2V ("++look morphoMap fun lemma "V"++") noPrep noPrep"),
  ("V3"     , \morphoMap fun lemma -> "mkV3 ("++look morphoMap fun lemma "V"++") noPrep noPrep"),
  ("VA"     , \morphoMap fun lemma -> "mkVA ("++look morphoMap fun lemma "V"++")"),
  ("VQ"     , \morphoMap fun lemma -> "mkVQ ("++look morphoMap fun lemma "V"++")"),
  ("VS"     , \morphoMap fun lemma -> "mkVS ("++look morphoMap fun lemma "V"++")"),
  ("VV"     , \morphoMap fun lemma -> "mkVV ("++look morphoMap fun lemma "V"++")"),
  ("Voc"    , \morphoMap fun lemma -> "ss \""++lemma++"\"")
  ]
  where
    look morphoMap fun lemma cat
      | contains "Masc_" fun = Map.findWithDefault ("mk"++cat++" \""++lemma++"\"") (lemma++"Masc_"++cat) morphoMap
      | contains "Fem_"  fun = Map.findWithDefault ("mk"++cat++" \""++lemma++"\"") (lemma++"Fem_"++cat) morphoMap
      | otherwise            = Map.findWithDefault ("mk"++cat++" \""++lemma++"\"") (lemma++"_"++cat) morphoMap

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

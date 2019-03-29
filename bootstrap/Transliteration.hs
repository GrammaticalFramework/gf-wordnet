module Transliteration(readTransliteration) where

import Data.Char
import Data.Maybe
import qualified Data.Map.Strict as Map

readTransliteration :: FilePath -> IO (String -> String)
readTransliteration fpath = do
  transl <- fmap toTranslitEntries $ readFile fpath
  return (transliterate transl)

transliterate transl cs =
  concat [fromMaybe [c] (Map.lookup c transl) | c <- cs]

toTranslitEntries =
  addCase .
  Map.fromList .
  map (toTuple . tsv) .
  lines
  where
    toTuple [[c],t] = (c,t)

    addCase transl =
      let transl' = Map.fromList [(c,fromMaybe [c] (Map.lookup (toLower c) transl)) | c <- [minBound..maxBound], c /= toLower c]
      in Map.union transl transl'

tsv :: String -> [String]
tsv "" = []
tsv cs =
  let (x,cs1) = break (=='\t') cs
  in x : if null cs1 then [] else tsv (tail cs1)

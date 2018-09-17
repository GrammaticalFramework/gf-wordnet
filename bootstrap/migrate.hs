import System.Environment
import qualified Data.Map as Map
import Data.Maybe

main = do
  [from_file,to_file] <- getArgs
  dict <- fmap (Map.fromList . mapMaybe toDictEntry . lines) $ readFile from_file
  mapp <- fmap (map toMapEntry . lines) $ readFile "bootstrap/mapping.txt"
  writeFile to_file (unlines ["lin "++new_id++" "++fromMaybe "= variants {} ;" (Map.lookup old_id dict) | (old_id,new_id) <- mapp])

toDictEntry l =
  let ws = words l
  in if take 1 ws == ["lin"]
       then Just (ws !! 1, unwords (drop 2 ws))
       else Nothing

toMapEntry l =
  let ws = words l
  in (ws !! 0,ws !! 1)

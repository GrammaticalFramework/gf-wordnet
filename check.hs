import Data.Maybe
import Data.List
import Data.Char
import qualified Data.Map as Map
import PGF2
import System.IO
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad(forM_)

main = do
  args    <- getArgs
  checked <- case args of
               []    -> do es <- fmap (catMaybes . map (readExpr . drop 5) . filter (\l -> take 4 l == "abs:") . lines) $ readFile "examples.txt"
                           let ids = nub (concatMap lemmas es)
                           return [("ParseEng",ids),("ParseSwe",ids),("ParseBul",ids)]
               ["-"] -> do ls <- fmap lines $ getContents
                           let res = [(lang,[(id,Just (unwords ws))]) | l <- ls, let (id:lang:ws) = words l]
                           return ((Map.toList . Map.fromListWith (++)) res) 
  forM_ checked $ \(lang,ids) ->
    if null ids
      then return ()
      else do let fname = "WordNet"++drop 5 lang++".gf"
              ls <- fmap lines $ readFile fname
              (tmp_fname,hTmp) <- openTempFile "." fname
              mapM_ (hPutStrLn hTmp . annotate ids) ls 
              hClose hTmp
              renameFile tmp_fname fname
  where
    lemmas e =
      case unApp e of
        Just (f,[]) -> [(f,Nothing)]
        Just (f,es) -> concatMap lemmas es
        _           -> []

annotate checked l =
  case words l of
    ("lin":id:"=":_) -> case lookup id checked of
                          Just def -> (reverse . dropWhile (/=';') . reverse) l
                          Nothing  ->                                         l
    _                              ->                                         l

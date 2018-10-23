module EM(EMState, DepTree,
          newEMState, freeEMState,
          setupUnigramSmoothing, setupPreserveTrees, setupRankingCallbacks,
          importTreebank, loadModel, exportAnnotatedTreebank,
          getBigramCount, getUnigramCount,
          step, dump) where

import PGF2
import Data.Maybe
import Foreign
import Foreign.C
import Matching
import System.IO.Unsafe(unsafePerformIO)

newtype EMState = EMState (Ptr ())

newEMState :: FilePath -> IO EMState
newEMState s = withCString s em_new_state
foreign import ccall em_new_state :: CString -> IO EMState

foreign import ccall "em_free_state" freeEMState :: EMState -> IO ()

foreign import ccall "em_setup_unigram_smoothing" setupUnigramSmoothing :: EMState -> Float -> IO ()
foreign import ccall "em_setup_preserve_trees" setupPreserveTrees :: EMState -> IO ()

importTreebank :: EMState -> FilePath -> String -> IO ()
importTreebank st fpath lang =
  withCString fpath $ \cpath ->
  withCString lang  $ \clang -> do
     res <- em_import_treebank st cpath clang
     if res == 0
       then fail "Loading failed"
       else return ()

foreign import ccall em_import_treebank :: EMState -> CString -> CString -> IO CInt

-- | Load a precomputed statistical model
loadModel :: EMState -> FilePath -> IO ()
loadModel st fpath =
  withCString fpath $ \cpath -> do
     res <- em_load_model st cpath
     if res == 0
       then fail "Loading failed"
       else return ()

foreign import ccall em_load_model :: EMState -> CString -> IO CInt


exportAnnotatedTreebank :: EMState -> FilePath -> IO ()
exportAnnotatedTreebank st fpath =
  withCString fpath $ \cpath -> do
     res <- em_export_annotated_treebank st cpath
     if res == 0
       then fail "Export failed"
       else return ()

foreign import ccall em_export_annotated_treebank :: EMState -> CString -> IO CInt

foreign import ccall "em_bigram_count" getBigramCount  :: EMState -> IO CInt
foreign import ccall "em_unigram_count" getUnigramCount :: EMState -> IO CInt

type RankingCallback = SenseChoice -> Fields -> DepTree -> Ptr CInt -> IO ()

setupRankingCallbacks :: EMState -> [(Cat,SenseChoice -> Fields -> DepTree -> IO Stat)] -> IO ()
setupRankingCallbacks st callbacks =
  mapM_ setRankingCallback callbacks
  where
    setRankingCallback :: (Cat,SenseChoice -> Fields -> DepTree -> IO Stat) -> IO ()
    setRankingCallback (cat,f) = do
      f <- wrapRankingCallback (\choice fields dtree res_c -> do
                                    (S res c) <- f choice fields dtree
                                    pokeElemOff res_c 0 res
                                    pokeElemOff res_c 1 c)
      withCString cat (\cat -> em_set_ranking_callback st cat f)

foreign import ccall em_set_ranking_callback :: EMState -> CString -> FunPtr RankingCallback -> IO ()
foreign import ccall unsafe "wrapper" wrapRankingCallback :: RankingCallback -> IO (FunPtr RankingCallback)


foreign import ccall "em_step" step :: EMState -> IO Float

dump :: EMState -> FilePath -> FilePath -> IO ()
dump st uni bi =
  withCString uni $ \cuni ->
  withCString bi  $ \cbi  -> 
    em_dump st cuni cbi
foreign import ccall em_dump :: EMState -> CString -> CString -> IO ()


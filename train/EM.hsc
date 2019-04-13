module EM(EMState(..), DepTree,
          withEMState, setupRankingCallbacks,
          addDepTree, incrementCounts, annotateDepTree,
          importTreebank, loadModel, exportAbstractTreebank,
          getBigramCount, getUnigramCount,
          step, dump) where

import PGF2
import PGF2.Internal
import Data.Maybe
import Data.Tree
import Foreign
import Foreign.C
import Matching
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad
import Control.Exception

#include "em_core.h"

newtype EMState = EMState (Ptr ())

withEMState :: PGF -> Float -> Float -> (EMState -> IO a) -> IO  a
withEMState gr usmooth bsmooth = bracket (em_new_state (pgf gr) usmooth bsmooth) (\st -> em_free_state st >> touchPGF gr)

foreign import ccall em_new_state :: Ptr a -> Float -> Float -> IO EMState
foreign import ccall em_free_state :: EMState -> IO ()

addDepTree :: EMState -> Tree (Fun,String) -> IO ()
addDepTree st t = do
  (_,dtree) <- mkRoot 0 (DepTree nullPtr) t
  em_add_dep_tree st dtree
  where
    mkRoot index parent (Node (fun,lbl) ts) = do
      dtree <- withCString fun  $ \cfun ->
                withCString lbl  $ \clbl ->
                 em_new_dep_tree st parent cfun clbl index (fromIntegral (length ts))
      let DepTree ptr = dtree
      index <- mkChildren dtree (ptr `plusPtr` (#offset DepTree, child)) (index+1) ts
      return (index, dtree)

    mkChildren parent ptr index []     = return index
    mkChildren parent ptr index (t:ts) = do
      (index,dtree) <- mkRoot index parent t
      poke ptr dtree
      mkChildren parent (ptr `plusPtr` (#size DepTree*)) index ts

foreign import ccall em_new_dep_tree :: EMState -> DepTree -> CString -> CString -> CSize -> CSize -> IO DepTree
foreign import ccall em_add_dep_tree :: EMState -> DepTree -> IO ()

incrementCounts :: EMState -> Expr -> Int -> IO ()
incrementCounts state e index = traverse e
  where
    traverse e =
      case unApp e of
        Just (fun,es) 
          | not (null es) -> do withCString fun $ \cfun ->
                                  em_increment_count state cfun (fromIntegral index)
                                mapM_ traverse es
        _                 -> return ()

foreign import ccall "em_increment_count" em_increment_count :: EMState -> CString -> CSize -> IO ()

annotateDepTree :: EMState -> DepTree -> IO [(CSize, Fun, Float)]
annotateDepTree state dtree =
  bracket gu_new_pool gu_pool_free $ \pool -> do
    buf <- em_annotate_dep_tree state dtree pool
    seq   <- (#peek GuBuf, seq) buf
    c_len <- (#peek GuSeq, len) seq
    peekElems (c_len :: (#type size_t)) (seq `plusPtr` (#offset GuSeq, data))
  where
    peekElems 0   ptr = return []
    peekElems len ptr = do
      index  <- (#peek EMLemmaProb, index) ptr
      fun    <- (#peek EMLemmaProb, fun)   ptr >>= peekCString
      prob   <- (#peek EMLemmaProb, prob)  ptr
      es <- peekElems (len-1) (ptr `plusPtr` (#size EMLemmaProb))
      return ((index,fun,prob):es)


foreign import ccall em_annotate_dep_tree :: EMState -> DepTree -> Ptr () -> IO (Ptr ())

foreign import ccall unsafe "gu/mem.h gu_new_pool"
  gu_new_pool :: IO (Ptr ())

foreign import ccall unsafe "gu/mem.h gu_pool_free"
  gu_pool_free :: Ptr () -> IO ()

importTreebank :: EMState -> String -> FilePath -> IO ()
importTreebank st lang fpath =
  withCString lang  $ \clang -> 
  withCString fpath $ \cpath -> do
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


exportAbstractTreebank :: EMState -> FilePath -> IO ()
exportAbstractTreebank st fpath =
  withCString fpath $ \cpath -> do
     res <- em_export_abstract_treebank st cpath
     if res == 0
       then fail "Export failed"
       else return ()

foreign import ccall em_export_abstract_treebank :: EMState -> CString -> IO CInt

foreign import ccall "em_bigram_count" getBigramCount  :: EMState -> IO CSize
foreign import ccall "em_unigram_count" getUnigramCount :: EMState -> IO CSize

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


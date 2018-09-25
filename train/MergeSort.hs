module MergeSort(mergesort) where

import Data.Array.IO
import System.Random
import Control.Monad

-- An imperative in-place implementation of merge sort.
--
-- The first array is the array of keys to be sorted, the second
-- is an array of weights - one weight for every key. Both the keys and 
-- the weights are sorted in the decreasing order of weights.
--
-- The first integer is the index of the first key/weight to be sorted.
-- The second integer is the index after the last key/weight.
mergesort :: IOArray Int k -> IOUArray Int Float -> Int -> Int -> IO ()
mergesort k_arr c_arr lo hi
  | hi - lo < 2 = return ()
  | otherwise   = do
      let mi = lo + (hi - lo) `div` 2;
          wo = lo + hi - mi;
      wsort lo mi wo
      expand lo hi wo
  where
    expand lo hi wo
      | wo - lo <= 2 = do
          when (wo   > lo) $ insert wo
          when (wo-1 > lo) $ insert (wo-1)
      | otherwise    = do
          let wo' = lo + (wo - lo + 1) `div` 2
          wsort wo' wo lo
          wmerge lo (lo + wo - wo') wo hi wo'
          expand lo hi wo'
      where
        insert i
          | i >= hi   = return ()
          | otherwise = do i0v <- readArray c_arr i
                           i1v <- readArray c_arr (i-1)
                           when (i0v > i1v) $ do
                             swap i (i-1)
                             insert (i+1)

    wsort lo hi wo
      | hi - lo > 1  = do let mi = lo + (hi - lo) `div` 2;
                          mergesort k_arr c_arr lo mi
                          mergesort k_arr c_arr mi hi
                          wmerge lo mi mi hi wo
      | hi - lo == 1 = swap lo wo
      | otherwise    = return ()

    wmerge i m j n wo = do
      (i,j,wo) <- swapCommon i j wo
      (i,wo)   <- swapLeft i wo
      (j,wo)   <- swapRight j wo
      return ()
      where
        swapCommon i j wo
          | i < m && j < n = do iv <- readArray c_arr i
                                jv <- readArray c_arr j
                                if iv > jv
                                  then swap wo i >> swapCommon (i+1) j     (wo+1)
                                  else swap wo j >> swapCommon i     (j+1) (wo+1)
          | otherwise      = return (i,j,wo)

        swapLeft i wo
          | i < m          = swap wo i >> swapLeft (i+1) (wo+1)
          | otherwise      = return (i,wo)

        swapRight j wo
          | j < n          = swap wo j >> swapRight (j+1) (wo+1)
          | otherwise      = return (j,wo)

    swap :: Int -> Int -> IO ()
    swap i j = do
      iv <- readArray k_arr i
      jv <- readArray k_arr j
      writeArray k_arr i jv
      writeArray k_arr j iv

      iv <- readArray c_arr i
      jv <- readArray c_arr j
      writeArray c_arr i jv
      writeArray c_arr j iv

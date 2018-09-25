{-# LANGUAGE BangPatterns #-}

module EM(em) where

import Data.Array
import Data.Array.IO
import Data.Array.Unsafe
import qualified Data.Set as Set
import System.IO
import MergeSort

------------------------------------------------------------
-- This function is the core of the algorithm. Here we use
-- expectation maximization to estimate the counts.
-- The first argument is a list of pairs. The first element
-- of every pair is the list of possible keys and the second
-- element is how many times we have seen this list of 
-- possibilities. The result is the estimated count for each
-- of the keys. The estimation continues until convergency, i.e.
-- until the KL divergency is less than kl_limit.
--
-- Note: the code is optimized for memory efficiency since
-- it must work with more that 50 000 000 latent variables.
-- Be careful if you do any changes here!

data K a = K {-# UNPACK #-} !Int a

em :: Ord k => [([k],Float)] -> Float -> Float -> IO ([(k,Float)], Float)
em xs kl_limit kl_cut = do
  keys   <- newListArray (0,size-1) (Set.toList keySet)
  old    <- newArray (0,size-1) (1 :: Float)
  new    <- newArray (0,size-1) (0 :: Float)
  counts <- loop 1 old new (map addIndices xs)
  if kl_cut > 0
    then do putStr "sorting ... " >> hFlush stdout
            mergesort keys counts 0 size
            putStrLn ""
            putStr "cutting ... " >> hFlush stdout
            (size,total) <- cuttingPoint counts
            putStrLn ""
            keys   <- unsafeFreeze keys
            counts <- unsafeFreeze counts
            return (zip (take size (elems keys)) (take size (elems counts)), total)
    else do keys   <- unsafeFreeze keys
            counts <- unsafeFreeze counts
            return (zip (elems keys) (elems counts), total)
  where
    keySet = Set.fromList (concatMap fst xs)
    size   = Set.size keySet

    total  = sum (map snd xs) :: Float

    addIndices (ks,c) = (map (\k -> K (Set.findIndex k keySet) k) ks, c)

    loop :: Int -> IOUArray Int Float -> IOUArray Int Float -> [([K k],Float)] -> IO (IOUArray Int Float)
    loop n old new xs = do
      iter xs
      kl <- divergency
      clean
      putStr ("\r\x1b[0K" ++ show n++" "++show kl) >> hFlush stdout
      if abs kl < kl_limit 
        then putStrLn "" >> return new
        else loop (n+1) new old xs
      where
        iter :: [([K k],Float)] -> IO ()
        iter []          = return ()
        iter ((fs,c):xs) = do total <- sumup fs
                              addCount total fs
                              iter xs
          where
            addCount :: Float -> [K k] -> IO ()
            addCount total []           = return ()
            addCount total (K i f : fs) = do
              old_c <- readArray old i
              new_c <- readArray new i
              writeArray new i (new_c + (old_c/total)*c)
              addCount total fs

        sumup = compute 0
          where
            compute :: Float -> [K k] -> IO Float
            compute !s []           = return s
            compute !s ((K i f):fs) = do
              old_c <- readArray old i
              compute (s + old_c) fs

        divergency = sum 0 0
          where
            sum :: Int -> Float -> IO Float
            sum !i !kl
              | i >= size = return kl
              | otherwise = do
                  old_c <- readArray old i
                  new_c <- readArray new i
                  if abs(old_c-new_c)/total > 1e-50
                    then sum (i+1) (kl + (old_c*log(old_c/new_c)/total))
                    else sum (i+1) kl

        clean = set 0
          where
            set :: Int -> IO ()
            set !i
              | i >= size = return ()
              | otherwise = do
                  writeArray old i 0
                  set (i+1)

    cuttingPoint :: IOUArray Int Float -> IO (Int,Float)
    cuttingPoint counts = cut 0 0
       where
         max = total * exp(-kl_cut)

         cut :: Int -> Float -> IO (Int,Float)
         cut i sum
           | i >= size = return (size,sum)
           | otherwise = do c <- readArray counts i
                            let sum' = sum + c
                            if sum' > max
                              then return (i,sum)
                              else cut (i+1) sum'

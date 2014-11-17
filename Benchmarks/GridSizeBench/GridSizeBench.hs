
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding ( replicate, zipWith ) 
import qualified Prelude as P


import Obsidian
import Obsidian.Run.CUDA.Exec
-- import Obsidian.Run.CUDA.SC

import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Int
import Data.Word

import System.Environment
import System.CPUTime.Rdtsc (rdtsc)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Data.IORef

import Data.Time.Clock

-- ######################################################################
-- Reduction Kernel 
-- ######################################################################


red3 :: Storable a
           => Word32 
           -> (a -> a -> a)
           -> Pull Word32 a
           -> Program Block a
red3 cutoff f  arr
  | len arr == cutoff =
    return (fold1 f arr ! 0) 
  | otherwise = 
    do
      let (a1,a2) = halve arr
      arr' <- forcePull (zipWith f a1 a2)
      red3 cutoff f arr'   


mapRed3 :: Storable a => Word32 -> Word32 -> (a -> a -> a) -> DPull a -> DPush Grid a
mapRed3 blocksize seq_depth f arr = pConcat $ fmap sConcat (fmap (fmap body) arr')
  where
    body arr = singletonPush (red3 2 f arr)
    arr' =  fmap (splitUp (blocksize `div` seq_depth)) (splitUp blocksize arr)
     

-- ######################################################################
-- Main
-- ######################################################################

-- these remain constant over benchspace
data_size = 8388608
results_size = 16384 

-- This varies (this is the benchspace) 
-- grid_size -> (elementsperblock,seq depth mapping)
grid_sizes = [(16384,(512,1))
             ,(8192 ,(1024,2))
             ,(4096 ,(2048,4))
             ,(2048 ,(4096,8))
             ,(1024 ,(8192,16))
             ,(512  ,(16384,32))
             ,(256  ,(32768,64))
             ,(128  ,(65536,128))
             ,(64   ,(131072,256))
             ,(32   ,(262144,512))
             ,(16   ,(524288,1024))
             ,(8    ,(1048576,2048))
             ,(4    ,(2097152,4096))
             ,(2    ,(4194393,8192))
             ,(1    ,(8388608,16384))]


exitError = do
  error $ "Provide 1 arg: Grid size (one of: "  ++ show (map fst grid_sizes) ++ ")"

main :: IO ()
main = do
  putStrLn "Running GridSize benchmark.."
  args <- getArgs
  case length args of
    1 -> do
      let g_size = read $ args P.!! 0
      case P.lookup g_size grid_sizes of
        Nothing -> exitError 
        Just seq_depth -> performBench g_size seq_depth 
          
    _ -> exitError 

  where
    performBench g_size (blocksize,seq_depth) = 
      withCUDA $
      do lift $ putStrLn $ "  grid: " P.++ show g_size P.++ "\n  elt/block: " P.++ show blocksize P.++
                           "\n  seq_depth: " P.++ show seq_depth
          

         capt <-
           capture 512 (mapRed3 blocksize seq_depth (+))
                          
         --(inputs :: V.Vector Word32) <- lift $ mkRandomVec (fromIntegral (data_size))
         let (inputs :: V.Vector Word32) = V.fromList (P.replicate data_size 1 )
         
         useVector inputs $ \i ->
           allocaVector (fromIntegral results_size)  $ \(o :: CUDAVector Word32) ->
           do fill o 0

              t0 <- lift getCurrentTime
              cnt0 <- lift rdtsc
              forM_ [0..999::Int] $ \_ -> do
                o <== (g_size,capt) <> i
                syncAll
              cnt1 <- lift rdtsc
              t1 <- lift getCurrentTime

              r <- peekCUDAVector o
              lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
              lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)

              lift $ putStrLn $ show $ P.take results_size  r 

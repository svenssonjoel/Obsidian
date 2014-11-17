
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
-- Nonsense Kernel 
-- ######################################################################
nonsense :: (Storable a, Num a) => 
            Pull Word32 a
           -> BProgram (Push Block Word32 a)
nonsense arr = do 
  arr1 <- forcePull $ fmap (+1) arr
  arr2 <- forcePull $ fmap (*2) arr1
  arr3 <- forcePull $ fmap (*4) arr2
  return $ push arr3 


mapNonsense :: (Storable a, Num a)  => Word32 -> Word32 -> DPull a -> DPush Grid a
mapNonsense blocksize seq_depth arr = pConcat $ fmap sConcat (fmap (fmap body) arr')
  where
    body = runPush . nonsense 
    arr' =  fmap (splitUp (blocksize `div` seq_depth)) (splitUp blocksize arr)

-- ######################################################################
-- Nonsense Kernel 
-- ######################################################################


kernels :: [(String, (Word32,Word32 -> Word32 -> DPull EWord32 -> DPush Grid EWord32))]
kernels = [("REDUCTION", (16384,(\b s -> mapRed3 b s (+))))
          ,("NONSENSE", (8388608,mapNonsense))]
          


-- ######################################################################
-- Main
-- ######################################################################

-- these remain constant over benchspace
data_size = 8388608
--results_size = 16384 

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
  error $ "Provide 2 args: Benchmark [\"REDUCTION\",\"NONSENSE\"] and Grid size (one of: "  ++ show (map fst grid_sizes) ++ ")"

main :: IO ()
main = do
  putStrLn "Running GridSize benchmark.."
  args <- getArgs
  case length args of
    2 -> do
      let g_size = read $ args P.!! 1
      case P.lookup g_size grid_sizes of
        Nothing -> exitError 
        Just seq_depth ->
          case P.lookup (args P.!! 0) kernels of
            Just k -> performBench k g_size seq_depth 
            Nothing -> exitError
    _ -> exitError 

  where
    performBench (k,results_size) g_size (blocksize,seq_depth) = 
      withCUDA $
      do lift $ putStrLn $ "  grid: " P.++ show g_size P.++ "\n  elt/block: " P.++ show blocksize P.++
                           "\n  seq_depth: " P.++ show seq_depth
          

         capt <-
           capture 512 (k blocksize seq_depth) -- (mapRed3 blocksize seq_depth (+))
                          
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

              lift $ putStrLn $ show $ P.take 1024 results_size  r 


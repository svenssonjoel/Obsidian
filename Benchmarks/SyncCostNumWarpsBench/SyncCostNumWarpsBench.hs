
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding ( replicate, zipWith ) 
import qualified Prelude as P


import Obsidian
import Obsidian.Run.CUDA.Exec

import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Word

import System.Environment
import System.CPUTime.Rdtsc (rdtsc)


import Data.Time.Clock
import Control.DeepSeq 

-- ######################################################################
-- Nonsense Kernel 
-- ######################################################################
nonsense :: (Data a, Num a)
            => Bool
            -> Pull Word32 a
            -> BProgram (Push Block Word32 a)
nonsense sync arr = loopit 10 arr  
  where loopit 0 arr = return $ push arr
        loopit n arr | sync = do
          arr' <- compute $ fmap (+1) arr
          loopit (n-1) arr'
                     | otherwise = do
          arr' <- unsafeWritePull False $ fmap (+1) arr
          loopit (n-1) arr' 


mapNonsense :: (Data a, Num a)  => Word32 -> Bool -> DPull a -> DPush Grid a
mapNonsense blocksize sync arr = asGridMap body arr'
  where
    body = execBlock . (nonsense sync)
    arr' = splitUp blocksize arr


-- ######################################################################
-- Main
-- ######################################################################

-- these remain constant over benchspace
data_size = 32768
grid_size = 32
block_size = 1024 -- number of elts  
results_size = data_size

exitError = do
  error $ "Provide 2 args: Benchmark [\"SYNC\",\"NOSYNC\"] and Number of warps " ++ show [1..32::Int] 

main :: IO ()
main = do
  putStrLn "Running SyncCost benchmark.."
  args <- getArgs
  case length args of
    2 -> do
      let sync = (args P.!! 0) == "SYNC"
          warps = read (args P.!! 1)
          threads = warps * 32 
      performBench sync threads 
    _ -> exitError 

  where
    performBench sync threads = 
      withCUDA $
      do  
         compile_t0 <- lift getCurrentTime 
         capt <-
           capture threads (mapNonsense block_size sync) 
         compile_t1 <- lift getCurrentTime 
           
         --(inputs :: V.Vector Word32) <- lift $ mkRandomVec (fromIntegral (data_size))
         let (inputs :: V.Vector Word32) = V.fromList (P.replicate data_size 0 )

         _ <- inputs `deepseq` return () 

         transfer_start <- lift getCurrentTime
         useVector inputs $ \i ->
           allocaVector (fromIntegral results_size)  $ \(o :: CUDAVector Word32) ->
           do transfer_done <- lift getCurrentTime 
  
              fill o 0

              t0 <- lift getCurrentTime
              cnt0 <- lift rdtsc
              forM_ [0..999::Int] $ \_ -> do
                o <== (grid_size,capt) <> i
                syncAll
              cnt1 <- lift rdtsc
              t1 <- lift getCurrentTime

              --r <- peekCUDAVector o
              r <- copyOut o
              t_end <- lift getCurrentTime

              
              lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
              lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)

              lift $ putStrLn $ "COMPILATION_TIME: " ++ show (diffUTCTime compile_t1 compile_t0)
    
              lift $ putStrLn $ "BYTES_TO_DEVICE: " ++ show (data_size * sizeOf (undefined :: EWord32))
              lift $ putStrLn $ "BYTES_FROM_DEVICE: " ++ show (fromIntegral results_size * sizeOf (undefined :: EWord32))
              lift $ putStrLn $ "TRANSFER_TO_DEVICE: " ++ show (diffUTCTime transfer_done transfer_start)
              lift $ putStrLn $ "TRANSFER_FROM_DEVICE: " ++ show (diffUTCTime t_end t1)

              lift $ putStrLn $ "ELEMENTS_PROCESSED: " ++ show data_size
              lift $ putStrLn $ "NUMBER_OF_BLOCKS: " ++ show grid_size
              lift $ putStrLn $ "ELEMENTS_PER_BLOCK: " ++ show block_size
              lift $ putStrLn $ show $ P.take 10 (V.toList r) 


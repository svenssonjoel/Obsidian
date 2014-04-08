
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reduce

import Prelude hiding (replicate)
import Prelude as P


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
-- Tools
-- ######################################################################
cyclesPerSecond :: IO Word64
cyclesPerSecond = do
  t1 <- rdtsc
  --threadDelay 10
  t2 <- rdtsc
  return $ t2 - t1


-- ######################################################################
-- Main
-- ######################################################################

kernels :: [(String, DPull (SPull (Exp Word32)) -> DPush Grid (Exp Word32))]
kernels = [("r1", mapRed1 (+))
          ,("r2", mapRed2 (+))
          ,("r3", mapRed3 (+))
          ,("r4", mapRed4 (+))
          ,("r5", mapRed5 (+))
          ,("r6", mapRed6 (+))
          ,("r7", mapRed7 (+))] 

exitError = do
  error "Provide 3 args: Kernel, ThreadsPerBlock, ElementsPerBlock" 

main :: IO ()
main = do
  putStrLn "Running reduction benchmark..." 
  args <- getArgs
  case (length args) of
      3 -> if (args P.!! 0 == "large")
           then largeBench args
           else smallBench args 
      _ -> exitError
        
  where
    smallBench args = do
      let k = args P.!! 0
          t = read $ args P.!! 1
          e = read $ args P.!! 2

      case (lookup k kernels) of
        Nothing   -> error "Incorrect kernel" 
        Just kern -> runBenchmark kern t e
    largeBench args = do
      let k = args P.!! 1
          t = read $ args P.!! 2

      case (lookup k kernels) of
        Nothing -> exitError
        Just kern -> runLargeBenchmark kern t 

runBenchmark :: (Pull EWord32 (SPull (Exp Word32)) -> DPush Grid (Exp Word32)) -> Word32 -> Word32 -> IO ()
runBenchmark origkern t elts =
  withCUDA $
  do
    capt <- capture t (origkern . splitUp elts)
    
    (inputs :: V.Vector Word32) <- lift $ mkRandomVec (fromIntegral (blcks * elts))
    
    let cpuresult = V.sum inputs 
    
    useVector inputs $ \i ->
      allocaVector (fromIntegral blcks)  $ \(o :: CUDAVector Word32) ->
        body cpuresult capt i o
        --allocaVector 1  $ \(o2 :: CUDAVector Word32) -> do 
                                                        
  where
    blcks = 8192
    body cpuresult kern i o = 
        do
          fill o 0
        

          t0   <- lift getCurrentTime
          cnt0 <- lift rdtsc
          forM_ [0..999] $ \_ -> do 
            o <== (blcks,kern) <> i
            syncAll
          cnt1 <- lift rdtsc
          t1   <- lift getCurrentTime

          r <- peekCUDAVector o
          lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
          lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)

          when (sum r /= cpuresult) $ lift $
            putStrLn "WARNING: CPU and GPU results not equal!!"




runLargeBenchmark :: (Pull EWord32 (SPull (Exp Word32)) -> DPush Grid (Exp Word32)) -> Word32 -> IO ()
runLargeBenchmark origkern t = 
  withCUDA $
  do
    let elts = 4096
        
    capt <- capture t (origkern . splitUp elts)
    
    (inputs :: V.Vector Word32) <- lift $ mkRandomVec (fromIntegral (blcks * elts))
    
    let cpuresult = V.sum inputs 
    
    useVector inputs $ \i ->
      allocaVector (fromIntegral blcks)  $ \(o :: CUDAVector Word32) ->
        allocaVector 1 $ \out -> 
          body cpuresult capt i o out
        --allocaVector 1  $ \(o2 :: CUDAVector Word32) -> do 
                                                        
  where
    blcks = 4096
    body cpuresult kern i o out = 
        do
          fill o 0
        
          t0   <- lift getCurrentTime
          cnt0 <- lift rdtsc
          -- Scale up the benchmark 1000 times to make it take long enough.
          forM_ [0..999] $ \_ -> do 
            o <== (blcks,kern) <> i
            out <== (1,kern) <> o 
            syncAll
          cnt1 <- lift rdtsc
          t1   <- lift getCurrentTime

          r <- peekCUDAVector out
          lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
          lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)

          when (r P.!! 0 /= cpuresult) $ lift $ do 
            putStrLn "WARNING: CPU and GPU results not equal!!" 

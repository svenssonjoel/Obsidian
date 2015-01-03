
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

import Data.Word

import System.Exit
import System.Environment
import System.CPUTime.Rdtsc (rdtsc)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Data.Time.Clock
import Control.DeepSeq

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
      4 -> if (args P.!! 0 == "SPECIAL")
           then largeBench2 args
           else error "Wrong args" 
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


    largeBench2 args = do
      let threads  = read $ args P.!! 1
          elements = read $ args P.!! 2
          blocks   = read $ args P.!! 3
      runLargeBenchmark2 threads elements blocks 
          

runBenchmark :: (Pull EWord32 (SPull (Exp Word32)) -> DPush Grid (Exp Word32)) -> Word32 -> Word32 -> IO ()
runBenchmark origkern t elts =
  withCUDA $
  do
    compile_t0 <- lift getCurrentTime 
    capt <- capture t (origkern . splitUp elts)
    compile_t1 <- lift getCurrentTime
    
    let blcks = 8192

    (inputs :: V.Vector Word32) <- lift $ mkRandomVec (fromIntegral (blcks * elts))
    let cpuresult = V.sum inputs 

    _ <- cpuresult `deepseq` return () 

    transfer_start <- lift getCurrentTime
    useVector inputs $ \i ->
      allocaVector (fromIntegral blcks)  $ \(o :: CUDAVector Word32) -> do
        transfer_done <- lift getCurrentTime 
        --body cpuresult capt i o
        --allocaVector 1  $ \(o2 :: CUDAVector Word32) -> do 
        fill o 0
        

        t0   <- lift getCurrentTime
        cnt0 <- lift rdtsc
        forM_ [0..999] $ \_ -> do 
          o <== (blcks,capt) <> i
          syncAll
        cnt1 <- lift rdtsc
        t1   <- lift getCurrentTime

        r <- copyOut o 
        t_end <- lift getCurrentTime
        
        lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
        lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)

        lift $ putStrLn $ "COMPILATION_TIME: " ++ show (diffUTCTime compile_t1 compile_t0)
    
        lift $ putStrLn $ "BYTES_TO_DEVICE: " ++ show (fromIntegral (blcks * elts)  * sizeOf (undefined :: EWord32))
        lift $ putStrLn $ "BYTES_FROM_DEVICE: " ++ show (fromIntegral blcks * sizeOf (undefined :: EWord32))
        lift $ putStrLn $ "TRANSFER_TO_DEVICE: " ++ show (diffUTCTime transfer_done transfer_start)
        lift $ putStrLn $ "TRANSFER_FROM_DEVICE: " ++ show (diffUTCTime t_end t1)

        lift $ putStrLn $ "ELEMENTS_PROCESSED: " ++ show (fromIntegral (blcks * elts))
        lift $ putStrLn $ "NUMBER_OF_BLOCKS: " ++ show (fromIntegral blcks)
        lift $ putStrLn $ "ELEMENTS_PER_BLOCK: " ++ show (fromIntegral elts) 

        let ok = sum (V.toList r) == cpuresult

        lift $ case ok of
          False -> do 
            putStrLn "WARNING: CPU and GPU results not the same!!"
            exitFailure
          True -> putStrLn "Success: GREAT!! CPU and GPU result are the same!"

  


---------------------------------------------------------------------------
-- LARGE
---------------------------------------------------------------------------
runLargeBenchmark :: (Pull EWord32 (SPull (Exp Word32)) -> DPush Grid (Exp Word32)) -> Word32 -> IO ()
runLargeBenchmark origkern t = 
  withCUDA $
  do
    let elts = 4096
        blcks = 4096
                 
    compile_t0 <- lift getCurrentTime 
    capt <- capture t (origkern . splitUp elts)
    compile_t1 <- lift getCurrentTime
    
    (inputs :: V.Vector Word32) <- lift $ mkRandomVec (fromIntegral (blcks * elts))
    
    let cpuresult = V.sum inputs

    _ <- cpuresult `deepseq` return () 

    transfer_start <- lift getCurrentTime
    useVector inputs $ \i ->
      allocaVector (fromIntegral blcks)  $ \(o :: CUDAVector Word32) ->
        allocaVector 1 $ \out -> 
          do transfer_done <- lift getCurrentTime 
             fill o 0
        
             t0   <- lift getCurrentTime
             cnt0 <- lift rdtsc
             -- Scale up the benchmark 1000 times to make it take long enough.
             forM_ [0..999 :: Int] $ \_ -> do 
               o <== (blcks,capt) <> i
               out <== (1,capt) <> o 
               syncAll
             cnt1 <- lift rdtsc
             t1   <- lift getCurrentTime


             r <- copyOut out 
             t_end <- lift getCurrentTime
                    
             lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
             lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)

             lift $ putStrLn $ "COMPILATION_TIME: " ++ show (diffUTCTime compile_t1 compile_t0)
          
             lift $ putStrLn $ "BYTES_TO_DEVICE: " ++ show (fromIntegral (blcks * elts)  * sizeOf (undefined :: EWord32))
             lift $ putStrLn $ "BYTES_FROM_DEVICE: " ++ show (fromIntegral blcks * sizeOf (undefined :: EWord32))
             lift $ putStrLn $ "TRANSFER_TO_DEVICE: " ++ show (diffUTCTime transfer_done transfer_start)
             lift $ putStrLn $ "TRANSFER_FROM_DEVICE: " ++ show (diffUTCTime t_end t1)

             lift $ putStrLn $ "ELEMENTS_PROCESSED: " ++ show (fromIntegral (blcks * elts))
             lift $ putStrLn $ "NUMBER_OF_BLOCKS: " ++ show (fromIntegral blcks)
             lift $ putStrLn $ "ELEMENTS_PER_BLOCK: " ++ show (fromIntegral elts) 

          
             let ok = (V.toList r) P.!! 0 == cpuresult

             lift $ case ok of
               False -> do 
                 putStrLn "WARNING: CPU and GPU results not the same!!"
                 exitFailure
               True -> putStrLn "Success: GREAT!! CPU and GPU result are the same!"



---------------------------------------------------------------------------
-- LARGE
---------------------------------------------------------------------------
runLargeBenchmark2 :: Word32 -> Word32 ->  Word32 -> IO ()
runLargeBenchmark2 t elements blocks = 
  withCUDA $
  do
    let

        elts  = elements `div` blocks 
        blcks = blocks -- 4096

        small_threads = blocks `div` 2

    let kern :: Word32 -> DPull (SPull (Exp Word32)) -> DPush Grid (Exp Word32)
        kern t = mapRed8 t (+)
                 
    compile_t0 <- lift getCurrentTime 
    capt <- capture t (kern t . splitUp elts)
    capt_small <- capture small_threads (kern small_threads . splitUp blocks)
    compile_t1 <- lift getCurrentTime
    
    (inputs :: V.Vector Word32) <- lift $ mkRandomVec (fromIntegral elements)
    
    let cpuresult = V.sum inputs

    _ <- cpuresult `deepseq` return () 

    transfer_start <- lift getCurrentTime
    useVector inputs $ \i ->
      allocaVector (fromIntegral blcks)  $ \(o :: CUDAVector Word32) ->
        allocaVector 1 $ \out -> 
          do transfer_done <- lift getCurrentTime 
             fill o 0
        
             t0   <- lift getCurrentTime
             cnt0 <- lift rdtsc
             -- Scale up the benchmark 1000 times to make it take long enough.
             forM_ [0..999 :: Int] $ \_ -> do 
               o <== (blcks,capt) <> i
               out <== (1,capt_small) <> o 
               syncAll
             cnt1 <- lift rdtsc
             t1   <- lift getCurrentTime


             r <- copyOut out 
             t_end <- lift getCurrentTime
                    
             lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
             lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)

             lift $ putStrLn $ "COMPILATION_TIME: " ++ show (diffUTCTime compile_t1 compile_t0)
          
             lift $ putStrLn $ "BYTES_TO_DEVICE: " ++ show (fromIntegral elements  * sizeOf (undefined :: EWord32))
             lift $ putStrLn $ "BYTES_FROM_DEVICE: " ++ show (fromIntegral blcks * sizeOf (undefined :: EWord32))
             lift $ putStrLn $ "TRANSFER_TO_DEVICE: " ++ show (diffUTCTime transfer_done transfer_start)
             lift $ putStrLn $ "TRANSFER_FROM_DEVICE: " ++ show (diffUTCTime t_end t1)

             lift $ putStrLn $ "ELEMENTS_PROCESSED: " ++ show (fromIntegral elements)
             lift $ putStrLn $ "NUMBER_OF_BLOCKS: " ++ show (fromIntegral blcks)
             lift $ putStrLn $ "ELEMENTS_PER_BLOCK: " ++ show (fromIntegral elts) 

          
             let ok = (V.toList r) P.!! 0 == cpuresult

             lift $ case ok of
               False -> do 
                 putStrLn "WARNING: CPU and GPU results not the same!!"
                 exitFailure
               True -> putStrLn "Success: GREAT!! CPU and GPU result are the same!"

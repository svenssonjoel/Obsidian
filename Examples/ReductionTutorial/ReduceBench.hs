
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
import System.Exit

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

kernels = [("r1", mapRed1 (+))
          ,("r2", mapRed2 (+))
          ,("r3", mapRed3 (+))
          ,("r4", mapRed4 (+))
          ,("r5", mapRed5 (+))
          ,("r6", mapRed6 (+))
          ,("r7", mapRed7 (+))] 

main = do
  putStrLn "Running reduction benchmark..." 
  args <- getArgs
  when (length args > 3 || length args < 3) $
    do
      putStrLn "Provide 3 args: Kernel, ThreadsPerBlock, ElementsPerBlock" 
      exitWith (ExitFailure 1)
  let k = args P.!! 0
      t = read $ args P.!! 1
      e = read $ args P.!! 2

  case (lookup k kernels) of
    Nothing ->
      do putStrLn "Incorrect kernel" 
         exitWith (ExitFailure 1)
    Just kern -> runBenchmark kern t e


runBenchmark kern t elts =
  withCUDA $
  do
    capt <- capture t (kern . splitUp elts)

    

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
        
<<<<<<< HEAD
          t0   <- lift getCurrentTime
          cnt0 <- lift rdtsc
          forM_ [0..999] $ \_ ->
            o <== (blcks,kern) <> i
          cnt1 <- lift rdtsc
          t1   <- lift getCurrentTime

=======

          t <- lift $ newIORef (0 :: Word64)

          ct0 <- lift getCurrentTime
          forM_ [0..999] $ \_ ->
            do 
              t0 <- o <==! (blcks,kern) <> i
              lift $ modifyIORef' t (\i -> i + t0) 
          ct1 <- lift getCurrentTime
    
>>>>>>> 2a9649dd0af388fc298731ccb8ef942046b72489
          r <- peekCUDAVector o
          when (sum r /= cpuresult) $ lift $ exitWith (ExitFailure 1) 

<<<<<<< HEAD
          lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
          lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)
=======
          t_tot <- lift $  readIORef t 
          lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime ct1 ct0)   -- ++ show t_tot 
>>>>>>> 2a9649dd0af388fc298731ccb8ef942046b72489

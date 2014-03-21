
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
import System.CPUTime.Rdtsc
import System.Exit

import Data.IORef

performSmall n threads r = 
  withCUDA $
  do
    kern <- capture threads (r (+) . splitUp n)

    (inputs :: V.Vector Word32) <- lift $ mkRandomVec (fromIntegral n)
   
    let cpuresult = V.sum inputs 
    
    useVector inputs $ \i ->
      allocaVector 1 $ \o -> 
      do
        o <== (1,kern) <> i 
        r <- peekCUDAVector o
        lift $ putStrLn $ show r
        lift $ putStrLn $ "compare CPU GPU results equal?: " ++ show ((r P.!! 0) == cpuresult)

all512 = [performSmall 512 256 mapRed1,
          performSmall 512 256 mapRed2,
          performSmall 512 256 mapRed3,
          performSmall 512 64 mapRed4,
          performSmall 512 64 mapRed5,
          performSmall 512 32 mapRed6,
          performSmall 512 16 mapRed7]

-- all4096 = [performSmall 4096 mapRed1,
--            performSmall 4096 mapRed2,
--            performSmall 4096 mapRed3,
--            performSmall 4096 mapRed4,
--            performSmall 4096 mapRed5,
--            performSmall 4096 mapRed6,
--            performSmall 4096 mapRed7]

           

performAll512 = sequence_ all512

--performAll4096 = sequence_ all4096

-- ######################################################################
-- Experiment (works only for specific blks/elts combos
performLarge blcks elts r  =
  withCUDA $
  do
    kern1 <- capture 32 (r (+) . splitUp elts)
    kern2 <- capture 64 (r (+) . splitUp elts)
    kern3 <- capture 96 (r (+) . splitUp elts)
    kern4 <- capture 128 (r (+) . splitUp elts)
    kern5 <- capture 160 (r (+) . splitUp elts)
    kern6 <- capture 192 (r (+) . splitUp elts)
    kern7 <- capture 256 (r (+) . splitUp elts)
                                           

    (inputs :: V.Vector Word32) <- lift $ mkRandomVec (fromIntegral (blcks * elts))
    
    let cpuresult = V.sum inputs 
    
    useVector inputs $ \i ->
      allocaVector (fromIntegral blcks)  $ \(o :: CUDAVector Word32) ->
        allocaVector 1  $ \(o2 :: CUDAVector Word32) -> do 

        body cpuresult kern1 i o o2
        body cpuresult kern2 i o o2
        body cpuresult kern3 i o o2
        body cpuresult kern4 i o o2
        body cpuresult kern5 i o o2
        body cpuresult kern6 i o o2
        body cpuresult kern7 i o o2
                                                        
  where
    body cpuresult kern i o o2 = 
        do
          fill o 0 
          o <== (blcks,kern) <> i
          o2 <== (1,kern) <> o 

          r <- peekCUDAVector o2
          lift $ putStrLn $ show r
          lift $ putStrLn $ "compare CPU GPU results equal?: " ++ show ((r P.!! 0) == cpuresult)



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
        

          t <- lift $ newIORef (0 :: Word64)
          
          forM_ [0..999] $ \_ ->
            do 
              t0 <- o <==! (blcks,kern) <> i
              lift $ modifyIORef t (\i -> i + t0) 


          r <- peekCUDAVector o
          when (sum r /= cpuresult) $ lift $ exitWith (ExitFailure 1) 
        

          t_tot <- lift $  readIORef t 
          lift $ putStrLn $ "SELFTIMED: " ++ show t_tot -- (t2 - t1) 

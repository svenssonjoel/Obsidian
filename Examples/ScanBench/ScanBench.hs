
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Scan

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

import Data.Time.Clock

-- ######################################################################
-- Tools
-- ######################################################################
imLog :: Integer->Integer->Integer
imLog b x
  = if x < b
    then 0
    else
      let
        l = 2 * imLog (b*b) x
        doDiv x l = if x < b then l else doDiv (x`div`b) (l+1)
      in
       doDiv (x`div`(b^l)) l


segmentedScan segSize vec | V.length vec == segSize = V.scanl1 (+) vec
                          | V.length vec < segSize = error "Incorrect vector size"
segmentedScan segSize vec = V.scanl1 (+) (V.take segSize vec) V.++
                            segmentedScan segSize (V.drop segSize vec) 
  
       
-- ######################################################################
-- Main
-- ######################################################################
main = do

  putStrLn "Running scan benchmark..."
  args <- getArgs
  when (length args /=  2) $
    do
      putStrLn "Provide 2 args: ThreadsPerBlock, ElementsPerBlock" 
      exitWith (ExitFailure 1)
  let t = read $ args P.!! 0
      e = read $ args P.!! 1
      blcks = 8192

  let eLog = fromIntegral $ imLog 2 (fromIntegral e) 
  
  withCUDA $ do
    capt <- capture t (mapScan eLog (+) . splitUp e)

    (inputs :: V.Vector Word32) <- lift $ mkRandomVec (fromIntegral (e * blcks))
    let cpuresult = segmentedScan (fromIntegral e) (V.map (`mod` 32) inputs)
          -- V.scanl1 (+) (V.map (`mod` 32) inputs)
    
    useVector (V.map (`mod` 32) inputs) $ \i -> -- (V.fromList (P.replicate (fromIntegral e) 1))  $ \i ->
      allocaVector (fromIntegral (e*blcks))  $ \(o :: CUDAVector Word32) -> do 
        fill o 0


        t0   <- lift getCurrentTime
        cnt0 <- lift rdtsc
        forM_ [1] $ \_ -> do
          o <== (blcks,capt) <> i
          syncAll
        cnt1 <- lift rdtsc
        t1   <- lift getCurrentTime


        
        r <- copyOut o -- peekCUDAVector o
        
        --lift $ putStrLn $ show r
        --lift $ putStrLn $ show cpuresult
        --lift $ putStrLn $ show (r == cpuresult)

        -- Computing the cpuresults take a long time!
        -- I implemented a bad cpu segmented scan (uses V.++) 
        --lift $ putStrLn "Done: ... Comparing to CPU result" 
        --when (r /= cpuresult) $ lift $ exitWith (ExitFailure 1) 
        
        
        lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
        lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)


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

import Data.IORef

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

  -- runBenchmark kern t e

  let eLog = fromIntegral $ imLog 2 (fromIntegral e) 
  
  withCUDA $ do
    capt <- capture t (mapScan eLog (+) . splitUp e)
 
    useVector (V.fromList (P.replicate (fromIntegral e) 1))  $ \i ->
      allocaVector (fromIntegral e)  $ \(o :: CUDAVector Word32) -> do 
        fill o 0
          
        o <== (1,capt) <> i
              
        r <- peekCUDAVector o
        
        lift $ putStrLn $ show r

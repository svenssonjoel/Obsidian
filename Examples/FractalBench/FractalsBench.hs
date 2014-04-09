{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Fractals

import Prelude hiding (replicate, writeFile)
import Prelude as P


import Obsidian
import Obsidian.Run.CUDA.Exec
-- import Obsidian.Run.CUDA.SC

import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Int

import Data.ByteString as BS hiding (putStrLn, length)

import System.Environment
import System.CPUTime.Rdtsc
import System.Exit

import Data.Time.Clock

-- Vary number of threads/block and image size  
main = do
  putStrLn "Running fractals benchmark..."
  args <- getArgs
  when (length args /=  2) $
    do
      putStrLn "Provide 2 args: ThreadsPerBlock, ImageSize" 
      exitWith (ExitFailure 1)

  let t = read $ args P.!! 0
      s = read $ args P.!! 1 
    
  withCUDA $
    do
      kern <- capture t (mandel s) 
    
      allocaVector (fromIntegral (s*s)) $ \o -> 
        do

          t0   <- lift getCurrentTime
          cnt0 <- lift rdtsc
          forM_ [0..999] $ \_ -> do
            o <== (s,kern)
            syncAll
          cnt1 <- lift rdtsc
          t1   <- lift getCurrentTime
          
          r <- copyOut o 

          -- Still going via list !!! 
          lift $ BS.writeFile "fractal.out" (pack (V.toList r))

          lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
          lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)




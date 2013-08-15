{-# LANGUAGE ScopedTypeVariables #-}

module ReductionExec where

import Reduction

import Prelude hiding (replicate)
import Prelude as P


import Obsidian
import Obsidian.Run.CUDA.Exec

import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Int


performSmall =
  withCUDA $
  do
    kern <- capture (reduce (+) . splitUp 256) 

    useVector (V.fromList [0..511 :: Int32]) $ \i ->
      useVector (V.fromList [0,0 :: Int32]) $ \ o ->
      do
        o <== (2,kern) <> i 
        r <- peekCUDAVector o
        lift $ putStrLn $ show r 




performLarge =
  withCUDA $
  do
    kern <- capture (reduce (+) . splitUp 256) 

    useVector (V.fromList [0..65535 :: Int32]) $ \i ->
      allocaVector 256  $ \(o :: CUDAVector Int32) ->
        allocaVector 1  $ \(o2 :: CUDAVector Int32) -> 
        do
          
          o <== (256,kern) <> i
          o2 <== (1,kern) <> o 

          r <- peekCUDAVector o2
          lift $ putStrLn $ show r 


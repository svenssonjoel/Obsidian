{-# LANGUAGE ScopedTypeVariables #-}

module HistogramExec where

import Histogram

import Prelude hiding (replicate)
import Prelude as P


import Obsidian
import Obsidian.Run.CUDA.Exec

import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Word

-- allocaVector does not zero out the memory.
performSmall =
  withCUDA $
  do
    kern <- capture (histogram 256 256) 

    useVector (V.fromList [0..255 :: Word32]) $ \i ->
      allocaVector 255 $ \ (m :: CUDAVector Word32) ->
      do
        fill m 0
        exec $ (1,kern) <:> m <> i 
        r <- peekCUDAVector m
        lift $ putStrLn $ show r 
 



performLarge =
  withCUDA $
  do
    kern <- capture (histogram 256 256) 

    useVector (V.fromList [0..65535 :: Word32]) $ \i ->
      allocaVector 65536 $ \ (m :: CUDAVector Word32) ->
        do
          fill m 0 
          exec $ (256,kern) <:> m  <> i
          r <- peekCUDAVector m
          lift $ putStrLn $ show r 


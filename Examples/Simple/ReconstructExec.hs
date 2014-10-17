{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reconstruct

import Prelude hiding (replicate)
import Prelude as P


import Obsidian
import Obsidian.Run.CUDA.Exec

import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Word


perform =
  withCUDA $
  do
    kern <- capture 256 (reconstruct 1 256)


    -- input needs to be "one longer" (hence 256) 
    useVector (V.fromList [0..256 :: Word32]) $ \i -> 
      allocaVector 256 $ \ (o :: CUDAVector Word32) ->
      do
        o <== (1,kern) <> i
        r <- peekCUDAVector o
        lift $ putStrLn $ show r 
  where
    input :: DPull EWord32
    input = undefinedGlobal (variable "X")


main  = perform 

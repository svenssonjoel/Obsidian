{-# LANGUAGE ScopedTypeVariables #-}

module ScanExec where

import Scan

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
    kern <- capture (sklansky 8 (+) . splitUp 256) ( input :- ())

    useVector (V.fromList [0..255 :: Word32]) $ \i -> 
      allocaVector 256 $ \ (o :: CUDAVector Word32) ->
      do
        execute kern 1 i o
        r <- peekCUDAVector o
        lift $ putStrLn $ show r 
  where
    input :: DPull EWord32
    input = undefinedGlobal (variable "X")



performLarge =
  withCUDA $
  do
    kern <- capture (sklansky 8 (+) . splitUp 256) ( input :- ())

    useVector (V.fromList [0..65535 :: Word32]) $ \i ->
      allocaVector 65536 $ \ (o :: CUDAVector Word32) ->
        do
          fill o 0 
          execute kern 256 i o
          r <- peekCUDAVector o
          lift $ putStrLn $ show r 
  where
    input :: DPull EWord32
    input = undefinedGlobal (variable "X")


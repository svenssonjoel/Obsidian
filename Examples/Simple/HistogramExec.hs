{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Histogram

import Prelude hiding (replicate)
import Prelude as P

import Obsidian.Run.CUDA.Exec

import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Word

-- allocaVector does not zero out the memory.
performSmall :: IO () 
performSmall =
  withCUDA $
  do
    kern <- capture 256 (histogram 1 256) 

    useVector (V.fromList (P.replicate 256 1)) $ \i -> 
      allocaVector 256 $ \ (m :: CUDAVector Word32) ->
      do
        fill m 0
        exec $ (1,kern) <:> m <> i 
        r <- peekCUDAVector m
        lift $ putStrLn $ show r 
 


performLarge :: IO ()
performLarge =
  withCUDA $
  do
    kern <- capture 256 (histogram 256 256) 

    useVector (V.fromList [0..65535 :: Word32]) $ \i ->
      allocaVector 65536 $ \ (m :: CUDAVector Word32) ->
        do
          fill m 0 
          exec $ (1,kern) <:> m  <> i
          r <- peekCUDAVector m
          lift $ putStrLn $ show r 


main :: IO ()
main = performLarge

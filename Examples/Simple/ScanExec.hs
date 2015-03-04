{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Scan

import Prelude hiding (replicate)
import Prelude as P


import Obsidian
import Obsidian.Run.CUDA.Exec

import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Word


-- perform =
--   withCUDA $
--   do
--     kern <- capture 256  (sklansky 10 (+) . splitUp 1024) 

--     useVector (V.fromList [0..1023 :: Word32]) $ \i -> 
--       allocaVector 1024 $ \ (o :: CUDAVector Word32) ->
--       do
--         fill o 0
--         o <== (1,kern) <> i 
--         r <- peekCUDAVector o
--         lift $ putStrLn $ show r 

perform =
  withCUDA $
  do
    kern <- capture 512  (sklanskies' 9 (+) 0 . splitUp 1024) 

    useVector (V.fromList [0..1023 :: Word32]) $ \i -> 
      allocaVector 1024 $ \ (o :: CUDAVector Word32) ->
      do
        fill o 0
        o <== (1,kern) <> i 
        r <- peekCUDAVector o
        lift $ putStrLn $ show r 



performKS =
  withCUDA $
  do
    kern <- capture 256  (ks 10 (+) . splitUp 1024) 

    useVector (V.fromList [0..1023 :: Word32]) $ \i -> 
      allocaVector 1024 $ \ (o :: CUDAVector Word32) ->
      do
        fill o 0
        o <== (1,kern) <> i 
        r <- peekCUDAVector o
        lift $ putStrLn $ show r 


-- Does not perform a "large" scan, but many small ones. 
performLarge =
  withCUDA $
  do
    kern <- capture 256 (sklansky 8 (+) . splitUp 256) 

    useVector (V.fromList (P.replicate 65536 1)) $ \i -> 
      allocaVector 65536 $ \ (o :: CUDAVector Word32) ->
        do
          fill o 0 
          o <== (256,kern) <> i
          r <- peekCUDAVector o
          lift $ putStrLn $ show r 



main = perform 

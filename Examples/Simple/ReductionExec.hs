{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reduction

import Prelude hiding (replicate)
import Prelude as P


import Obsidian
import Obsidian.Run.CUDA.Exec
-- import Obsidian.Run.CUDA.SC

import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Int


performSmall =
  withCUDA $
  do
    kern <- capture 128 (reduce (+)) --  . splitUp 512) 

    useVector (V.fromList [0..4095 :: Int32]) $ \i ->
      allocaVector 1 $ \ o -> 
      do
        o <== (1,kern) <> i 
        r <- peekCUDAVector o
        lift $ putStrLn $ show r 




-- performLarge =
--   withCUDA $
--   do
--     kern <- capture 128 (reduce (+)) --  . splitUp 256) 

--     useVector (V.fromList [0..65535 :: Int32]) $ \i ->
--       allocaVector 256  $ \(o :: CUDAVector Int32) ->
--         allocaVector 1  $ \(o2 :: CUDAVector Int32) -> 
--         do
--           fill o 0 
--           o <== (256,kern) <> i
--           o2 <== (1,kern) <> o 

--           r <- peekCUDAVector o2
--           lift $ putStrLn $ show r 


main = performSmall 

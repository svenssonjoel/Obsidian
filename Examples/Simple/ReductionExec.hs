{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
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
import Data.Word


performSmall =
  withCUDA $
  do
    --kern <- capture 128 (reduce (+)) --  . splitUp 512) 

    kern <- capture 4 (mapSumUp' . splitUp 8)         
--    kern <- capture 1 (mapSumUpT . (fmap (splitUp 8)) . splitUp 8)         
            
    useVector (V.fromList [0..15 :: Word32]) $ \i ->
      allocaVector 2 $ \ o -> 
      do
        o <== (1,kern) <> i 
        r <- peekCUDAVector o
        lift $ putStrLn $ show r 


type Kern = forall prg. CUDA (KernelT prg) 

red :: V.Vector Int32 -> IO (Int32)
red vec =
  withCUDA $
  do
    kern <- capture 256 (reduce (+))
    
    useVector vec $ \i ->
      allocaVector 4096 $ \(o :: CUDAVector Int32) ->
        allocaVector 1   $ \(o2 :: CUDAVector Int32) -> 
        do
          fill o 0 
          o <== (4096,kern) <> i
          o2 <== (1,kern) <> o 

          out <- copyOut o2
          return (out V.! 0)

performRed =
  let vec = V.fromList (P.replicate (2^24) 1)
  in  red vec 

performLarge =
  withCUDA $
  do
    kern <- capture 128 (reduce (+)) --  . splitUp 256) 

    useVector (V.fromList [0..65535 :: Int32]) $ \i ->
      allocaVector 256  $ \(o :: CUDAVector Int32) ->
        allocaVector 1  $ \(o2 :: CUDAVector Int32) -> 
        do
          fill o 0 
          o <== (256,kern) <> i
          o2 <== (1,kern) <> o 

          r <- peekCUDAVector o2
          lift $ putStrLn $ show r 


main = performLarge 

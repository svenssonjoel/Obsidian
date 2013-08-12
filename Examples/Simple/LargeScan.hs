{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Scan
import Reduction

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
  
    scanI <- capture (\a b -> sklanskyInc 8 (+) a (splitUp 256 b)) (variable "x" :- input :- ())
    reduce <- capture (reduce (+) . splitUp 256) (input :- ()) 
    scanCin <- capture kernel (input :- input :- ())

    useVector (V.fromList (P.replicate 65536 (1::Word32))) $ \i -> 
      allocaVector 256 $ \ (reds :: CUDAVector Word32) ->
        allocaVector 65536 $ \ (o :: CUDAVector Word32) ->
        do
          execute reduce 256 i reds
          
          execute scanI 1 ((0::Word32) :- reds) reds 
  
          execute scanCin 256 (reds :- i) o
          r <- peekCUDAVector o
          lift $ putStrLn $ show (P.take 256 r)
          lift $ putStrLn "..."
          lift $ putStrLn $ show (P.drop 65280 r) 
  where
    input :: DPull EWord32
    input = undefinedGlobal (variable "X")
    kernel cins arr = sklanskyCin 8 (+) cins (splitUp 256 arr)

                      

main = perform 

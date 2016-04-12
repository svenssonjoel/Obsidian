{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Reduction
import qualified ReduceIntro as RI

import Prelude hiding (replicate)
import Prelude as P


import Obsidian
import Obsidian.Run.CUDA.Exec

-- For OpenCL printing 
import qualified Obsidian.CodeGen.OpenCL as CL 

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


performSmallGeneric =
  withCUDA $ do
    kern <- capture 64 (RI.reduceGrid (+))

    useVector (V.fromList [0..1023 :: Int32]) $ \input ->
      allocaVector 2 $ \ o ->
      do o <== (1,kern) <> input
         r <- peekCUDAVector o
         lift $ putStrLn $ show r

performSmallNonGeneric =
  withCUDA $ do
    kern <- capture 64 (RI.reduceGrid' (+))

    useVector (V.fromList [0..1023 :: Int32]) $ \input ->
      allocaVector 2 $ \ o ->
      do o <== (1,kern) <> input
         r <- peekCUDAVector o
         lift $ putStrLn $ show r

performSmall1stage =
  withCUDA $ do
    kern <- capture 64 (RI.reduceGrid1stage (+))

    useVector (V.fromList [0..1023 :: Int32]) $ \input ->
      allocaVector 2 $ \ o ->
      do o <== (1,kern) <> input
         r <- peekCUDAVector o
         lift $ putStrLn $ show r


main = do

  let str = CL.genKernel 128 "reduce" (reduce (+) :: DPull (EInt32) -> DPush Grid (EInt32) )
  
  --performSmall

  putStrLn str 

  --performSmallGeneric
  --performSmallNonGeneric
  --performSmall1stage

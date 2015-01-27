{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import AsBlockMap

import Obsidian.Run.CUDA.Exec
import Control.Monad.State

import Data.Word

import qualified Data.Vector.Storable as V 

perform :: IO ()
perform =
  withCUDA $
  do
    kern <- capture 128 gridTest

    let inputs = V.fromList [0..128::Word32]
        
    useVector inputs $ \i ->
      allocaVector 128 $ \o ->
      do
        o <== (1,kern) <> i

        r <- peekCUDAVector o

        lift $ putStrLn $ show r 


main :: IO () 
main = perform 

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import MatMul

import Prelude hiding (replicate)
import Prelude as P


import Obsidian
import Obsidian.Run.CUDA.Exec
-- import Obsidian.Run.CUDA.SC

import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Int
import Data.Word


perform =
  withCUDA $
  do
    --kern <- capture 128 (reduce (+)) --  . splitUp 512)
    
    kern <- capture 32
            (\a b ->
              toDyn (matMul (fromDyn 32 (splitUp 32 a))
                            (fromDyn 32 (splitUp 32 b))))

            
    useVector (V.fromList (P.replicate (32*32) (2.0 :: Float))) $ \i1 ->
      useVector (V.fromList (P.replicate (32*32) (2.0 :: Float))) $ \i2 -> 
        allocaVector (32*32) $ \ o -> 
      do
        o <== (32,kern) <> i1 <> i2  
        r <- peekCUDAVector o
        lift $ putStrLn $ show r 


main = perform

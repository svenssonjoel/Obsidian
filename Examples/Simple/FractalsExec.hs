{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Fractals

import Prelude hiding (replicate, writeFile)
import Prelude as P


import Obsidian
import Obsidian.Run.CUDA.Exec
-- import Obsidian.Run.CUDA.SC

import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Int

import Data.ByteString as BS


perform :: IO ()
perform =
  withCUDA $
  do
    kern <- capture 256 mandel
    allocaVector (512*512) $ \o -> 
      do
        o <== (256,kern) 
        r <- copyOut o 

        lift $ BS.writeFile "fractal.out" (pack (V.toList r))



main = perform 

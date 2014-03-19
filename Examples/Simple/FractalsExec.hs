{-# LANGUAGE ScopedTypeVariables #-}

module FractalsExec where

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


perform =
  withCUDA $
  do
    kern <- capture 256 mandel --  . splitUp 512) 

    
--     useVector (V.fromList (P.replicate (512*512) 0)) $ \ o-> -- [0,0 :: Int32]) $ \ o ->
    allocaVector (512*512) $ \o -> 
      do
        o <== (512,kern) 
        r <- peekCUDAVector o

        -- lift $ putStrLn $ show r 
        lift $ BS.writeFile "fractal.out" (pack r)



main = perform 

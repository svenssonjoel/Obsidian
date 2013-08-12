{-# LANGUAGE ScopedTypeVariables #-}

module ReconstructExec where

import Reconstruct

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
    kern <- capture (reconstruct 255 255) ( input :- ())


    -- input needs to be "one longer" (hence 256) 
    useVector (V.fromList [0..256 :: Word32]) $ \i -> 
      allocaVector 256 $ \ (o :: CUDAVector Word32) ->
      do
        execute kern 1 i o
        r <- peekCUDAVector o
        lift $ putStrLn $ show r 
  where
    input :: DPull EWord32
    input = undefinedGlobal (variable "X")



-- performLarge =
--   withCUDA $
--   do
--     kern <- capture histogram (inputM :- input :- ())

--     useVector (V.fromList [0..65535 :: Word32]) $ \i ->
--       allocaVector 65536 $ \ (m :: CUDAVector Word32) ->
--         do
--           fill m 0 
--           execute kern 256 m i
--           r <- peekCUDAVector m
--           lift $ putStrLn $ show r 


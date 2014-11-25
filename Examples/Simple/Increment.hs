
{-# LANGUAGE ScopedTypeVariables #-}


module Main where


import Obsidian
import Obsidian.Run.CUDA.Exec

import Prelude hiding (map, zipWith, sum, replicate, take, drop, iterate, reverse)
import qualified Data.Vector.Storable as V

import Control.Monad.State

---------------------------------------------------------------------------
-- Simple Increment example
---------------------------------------------------------------------------

incLocal :: SPull EWord32 -> SPull EWord32
incLocal arr = fmap (+1) arr


increment :: DPull (SPull EWord32) -> DPush Grid EWord32 
increment arr = asGridMap body arr
  where body a = push (incLocal a) :: SPush Block EWord32 


increment' :: DPull (SPull (SPull EWord32)) -> DPush Grid EWord32 
increment' arr = asGrid (fmap body arr) 
  where body a = asBlock (fmap body' a) 
        body' a  = push $ incLocal a :: SPush Block EWord32

performInc :: IO () 
performInc =
  withCUDA $
  do
    -- kern <- capture 256 (increment . splitUp 256)
    kern <- capture 32 (increment' . (fmap (splitUp 8)) . splitUp 256)
    

    useVector (V.fromList [0..511]) $ \i ->
      allocaVector 512  $ \o ->
      do
        fill o 0

        o <== (1,kern) <> i

        r <- copyOut o
        lift $ putStrLn $ show r

main :: IO () 
main = performInc 

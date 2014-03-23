
{-# LANGUAGE ScopedTypeVariables #-}


module Increment where


import Obsidian
import Obsidian.Run.CUDA.Exec


import Data.Word
import Data.Int

import Prelude hiding (map, zipWith, sum, replicate, take, drop, iterate, reverse)
import Prelude as P

import qualified Data.Vector.Storable as V

import Control.Monad.State

---------------------------------------------------------------------------
-- Simple Increment example
---------------------------------------------------------------------------

incLocal :: SPull EWord32 -> SPull EWord32
incLocal arr = fmap (+1) arr


increment :: DPull (SPull EWord32) -> DPush Grid EWord32 
increment arr = pConcat (fmap body arr) 
  where body a = push (incLocal a)  


increment' :: DPull (SPull (SPull EWord32)) -> DPush Grid EWord32 
increment' arr = pConcat (fmap body arr) 
  where body a = tConcat (fmap (push . incLocal) a) 



performInc =
  withCUDA $
  do
    -- kern <- capture 256 (increment . splitUp 256)
    kern <- capture 8 (increment' . (fmap (splitUp 32)) . splitUp 256)
    

    useVector (V.fromList [0..511]) $ \i ->
      allocaVector 512  $ \o ->
      do
        fill o 0

        o <== (2,kern) <> i

        r <- copyOut o
        lift $ putStrLn $ show r

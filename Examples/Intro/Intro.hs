
{-# LANGUAGE ScopedTypeVariables #-}


module Main where


import Obsidian
import Obsidian.Run.CUDA.Exec

import Prelude hiding (map, zipWith, sum, replicate, take, drop, iterate, reverse)
import qualified Data.Vector.Storable as V

import Control.Monad.State

import Data.Word

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------


increment :: Num a => SPull a -> SPull a
increment arr = fmap (+1) arr

incrementAndReverse :: Num a => SPull a -> SPull a
incrementAndReverse = reverse . increment 


-- Programming the Hierarchy

-- incrementKernel :: Num a => DPull a -> DPush Grid a
-- incrementKernel arr = asGrid $ fmap (push . increment) arr'
--   where
--     -- make a selection of how many elements to process per CUDA block
--     arr' = splitUp 2048 arr 


---------------------------------------------------------------------------
-- Simple Increment example
---------------------------------------------------------------------------

-- incLocal :: SPull EWord32 -> SPull EWord32
-- incLocal arr = fmap (+1) arr


-- increment :: DPull (SPull EWord32) -> DPush Grid EWord32 
-- increment arr = asGridMap body arr
--   where body a = push (incLocal a) :: SPush Block EWord32 


-- increment' :: DPull (SPull (SPull EWord32)) -> DPush Grid EWord32 
-- increment' arr = asGrid (fmap body arr) 
--   where body a = asBlock (fmap body' a) 
--         body' a  = push $ incLocal a :: SPush Block EWord32

performInc :: IO () 
performInc =
  withCUDA $
  do
    -- kern <- capture 256 (increment . splitUp 256)
    kern <- capture 512 incrementKernel
    

    useVector (V.fromList [0..4096 :: Word32]) $ \i ->
      allocaVector 4096  $ \o ->
      do
        fill o 0

        o <== (2,kern) <> i

        r <- copyOut o
        lift $ putStrLn $ show r

main :: IO () 
main = performInc 

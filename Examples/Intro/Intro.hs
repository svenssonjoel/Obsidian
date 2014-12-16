
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

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

increment2 :: (t *<=* Block, Data a, Compute t, Num a) => SPull a -> Program t (SPull a)
increment2 arr = compute $ push $ fmap (+1) arr



-- Programming the Hierarchy
incrementKernel :: Num a => DPull a -> DPush Grid a
incrementKernel arr = liftGrid $ fmap (asBlock . increment) arr'
  where
    -- make a selection of how many elements to process per CUDA block
    arr' = splitUp 2048 arr 

incrementKernel2 :: (Data a, Num a) => DPull a -> DPush Grid a
incrementKernel2  arr =
  liftGrid $ fmap (execBlock . increment2) arr'
  where
    -- make a selection of how many elements to process per CUDA block
    arr' = splitUp 2048 arr 

incrementKernel3 :: (Data a, Num a) => DPull a -> DPush Grid a
incrementKernel3 arr =
  liftGrid $ fmap (liftBlock . (fmap (execWarp . increment2))) arr'
  where
    -- make a selection of how many elements to process per CUDA block/warp
    arr' = fmap (splitUp 512) $ splitUp 2048 arr 



--------------------------------------------------
-- run the kernel 
performInc :: IO () 
performInc =
  withCUDA $
  do
    kern <- capture 512 incrementKernel2
    
    useVector (V.fromList [0..4096 :: Word32]) $ \i ->
      allocaVector 4096  $ \o ->
      do
        fill o 0

        o <== (2,kern) <> i

        r <- copyOut o
        lift $ putStrLn $ show r

main :: IO () 
main = performInc 

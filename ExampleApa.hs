{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE GADTs #-}
module Examples where

import Obsidian
import Obsidian.CodeGen.CUDA


import Data.Word
import Data.Int
import Data.Bits
import qualified Data.Map as M

import qualified Data.Vector.Storable as V

import Control.Monad.State

import Prelude hiding (zipWith,sum,replicate,reverse )
import qualified Prelude as P

import Obsidian.Run.CUDA.Exec

mapFusion :: SPull EInt32 -> Program Block (SPush Block EInt32)
mapFusion arr =
  do
    imm <- forcePull $ (fmap (+1) . fmap (+1)) arr
    imm1 <- forcePull $ (fmap (+1) . fmap (+1)) imm
    return $ push imm1


mapFusion' :: (Forceable t, Pushable t)
              => SPull EInt32 -> (SPush t EInt32)
mapFusion' arr =
  runPush $ do
    imm <- forcePull $ (fmap (+1) . fmap (+1)) arr
    imm1 <- forcePull $ (fmap (+1) . fmap (+1)) imm
    return $ push imm1

input1 :: DPull EInt32
input1 = namedGlobal "apa" (variable "X")

mapKern :: DPull EInt32 -> DPush Grid EInt32 
mapKern arr = pConcat $ (fmap mapFusion' . splitUp 256)  arr

performKern =
  withCUDA $
  do
    kern <- capture 256 mapKern

    useVector (V.fromList [0..767::Int32]) $ \i ->
      allocaVector 768 $ \(o :: CUDAVector Int32)  ->
      do
        fill o 0 
        o <== (2,kern) <> i

        r <- peekCUDAVector o
        lift $ putStrLn $ show r 


---------------------------------------------------------------------------
-- Warp experiment
---------------------------------------------------------------------------


localReverse :: Pushable t => SPull EInt32 -> SPush t EInt32
localReverse arr = push . reverse $ arr

warpLocal :: SPull EInt32 -> SPush Warp EInt32
warpLocal arr =
  runPush ( do
   arr1 <- force $ (localReverse arr :: SPush Warp EInt32)  
   return $ push arr1 )
   --arr2 <- force $ warpLocal arr1
   --arr3 <- force  $ fmap (+100)  arr2
   --return $ push $ fmap (\x -> x-100) arr3
             

block :: SPull EInt32 -> SPush Block EInt32
block arr = pConcat $ fmap warpLocal (splitUp 100 arr)



grid :: DPull EInt32 -> DPush Grid EInt32
grid arr = pConcat $ fmap block (splitUp 500 arr)



-- genGrid = ppr $ compile PlatformCUDA (Config 256 1) "apa" (a,rim) 
--    where
--       (a,im) = toProgram_ 0 grid
--       iml = computeLiveness im
--       (m,mm) = mmIM iml sharedMem (M.empty)
--       rim = renameIM mm iml

-- genGrid2 = ppr $ compile PlatformCUDA (Config 256 1) "apa" (a,rim) 
--    where
--       (a,im) = toProgram_ 0 grid2
--       iml = computeLiveness im
--       (m,mm) = mmIM iml sharedMem (M.empty)
--       rim = renameIM mm iml


performGrid =
  withCUDA $
  do
    kern <- capture 96 grid

    useVector (V.fromList [0..500::Int32]) $ \i ->
      allocaVector 500 $ \(o :: CUDAVector Int32)  ->
      do
        fill o 0 
        o <== (1,kern) <> i

        r <- peekCUDAVector o
        lift $ putStrLn $ show r 




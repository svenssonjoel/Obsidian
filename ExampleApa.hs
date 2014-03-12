{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE GADTs #-}
module Examples where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Types
import Obsidian.Array
import Obsidian.Library
import Obsidian.LibraryG
import Obsidian.Force
import Obsidian.CodeGen.Reify
import Obsidian.CodeGen.CompileIM
import Obsidian.CodeGen.Memory
import Obsidian.CodeGen.Liveness
import Text.PrettyPrint.Mainland hiding ((<>) )

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
    imm <- force $ (fmap (+1) . fmap (+1)) arr
    imm1 <- force $ (fmap (+1) . fmap (+1)) imm
    return $ push imm1

local = pJoin 

mapFusion' :: (Pushable t, Write Pull EInt32 t)
              => SPull EInt32 -> (SPush t EInt32)
mapFusion' arr =
  local $ do
    imm <- force $ (fmap (+1) . fmap (+1)) arr
    imm1 <- force $ (fmap (+1) . fmap (+1)) imm
    return $ push imm1

input1 :: DPull EInt32
input1 = namedGlobal "apa" (variable "X")

mapKern :: DPull EInt32 -> DPush Grid EInt32 
mapKern arr = pConcat $ (fmap mapFusion' . splitUp 256)  arr



genKern = ppr $ compile PlatformCUDA (Config 256 1) "apa" (a,rim) 
  where
     (a,im) = toProgram_ 0 mapKern
     iml = computeLiveness im
     (m,mm) = mmIM iml sharedMem (M.empty)
     rim = renameIM mm iml

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


                             
     --  let WProgram g = f (h w) in g w  

warpLocal :: SPull EInt32 -> SPush Warp EInt32
warpLocal arr = push . reverse $ arr

warpLocal2 :: SPull EInt32 -> Program Warp (SPush Warp EInt32) 
warpLocal2 arr =
  do
   arr1 <- force $ warpLocal arr
   return $ push arr1
   --arr2 <- force $ warpLocal arr1
   --arr3 <- force  $ fmap (+100)  arr2
   --return $ push $ fmap (\x -> x-100) arr3
             

block :: SPull EInt32 -> SPush Block EInt32
block arr = pConcat $ fmap (\a -> warpLocal a)  (splitUp 32 arr)

block2 :: SPull EInt32 -> BProgram (SPush Block EInt32)
block2 arr =
  do
    arr1 <- force $ pConcat $ fmap (\a -> warpLocal a) (splitUp 32 arr)
    return $ pConcat $ fmap  (\a -> warpLocal a)  (splitUp 128 arr1) 


block3 :: SPull EInt32 -> SPush Block EInt32
block3 arr = pConcat $ fmap (\a -> wJoin $ warpLocal2 a) (splitUp 100 arr)




grid :: DPull EInt32 -> DPush Grid EInt32
grid arr = pConcat $ fmap block (splitUp 256 arr)

grid2 :: DPull EInt32 -> DPush Grid EInt32
grid2 arr = pConcat $ fmap block2 (splitUp 256 arr)


grid3 :: DPull EInt32 -> DPush Grid EInt32
grid3 arr = pConcat $ fmap  block3 (splitUp 500 arr)



genGrid = ppr $ compile PlatformCUDA (Config 256 1) "apa" (a,rim) 
   where
      (a,im) = toProgram_ 0 grid
      iml = computeLiveness im
      (m,mm) = mmIM iml sharedMem (M.empty)
      rim = renameIM mm iml

-- genGrid2 = ppr $ compile PlatformCUDA (Config 256 1) "apa" (a,rim) 
--    where
--       (a,im) = toProgram_ 0 grid2
--       iml = computeLiveness im
--       (m,mm) = mmIM iml sharedMem (M.empty)
--       rim = renameIM mm iml

genGrid3 = ppr $ compile PlatformCUDA (Config 32 1) "apa" (a,rim) 
   where
      (a,im) = toProgram_ 0 grid3
      iml = computeLiveness im
      (m,mm) = mmIM iml sharedMem (M.empty)
      rim = renameIM mm iml



performGrid =
  withCUDA $
  do
    kern <- capture 96 grid3

    useVector (V.fromList [0..500::Int32]) $ \i ->
      allocaVector 500 $ \(o :: CUDAVector Int32)  ->
      do
        fill o 0 
        o <== (1,kern) <> i

        r <- peekCUDAVector o
        lift $ putStrLn $ show r 




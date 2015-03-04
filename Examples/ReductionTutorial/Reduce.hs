
{-# LANGUAGE NoMonomorphismRestriction,
             ScopedTypeVariables#-}

module Reduce where

import Obsidian

-- import qualified Obsidian.CodeGen.CUDA as CUDA

import Data.Word
import Data.Int

import Control.Monad.State

import Prelude hiding (map,zipWith,sum,replicate,take,drop,iterate)
import qualified Prelude as P

import qualified Data.Vector.Storable as V
import Obsidian.Run.CUDA.Exec

input :: DPull EInt32
input = undefinedGlobal (variable "X")                                               

---------------------------------------------------------------------------
-- Kernel1  (Thread acceses element tid and tid+1 
---------------------------------------------------------------------------

red1 :: Data a
      => (a -> a -> a)
      -> Pull Word32 a
      -> Program Block (SPush Block a)
red1 f arr
  | len arr == 1 = return $ push $ arr 
  | otherwise    = 
    do
      let (a1,a2) = evenOdds arr
      imm <- computePull (zipWith f a1 a2)
      red1 f imm   

mapRed1 :: Data a => (a -> a -> a) -> Pull EWord32 (SPull a) -> Push Grid EWord32 a
mapRed1 f arr = liftGridMap body arr
  where
    body arr = execBlock (red1 f arr) 

getRed1 = putStrLn $
          genKernel 256 "red1"
            (mapRed1 (+) . splitUp 512 :: DPull EInt32 -> DPush Grid EInt32)


---------------------------------------------------------------------------
-- Kernel2 (Thread acceses element tid and tid+n )
---------------------------------------------------------------------------

red2 :: Data a 
        => (a -> a -> a)
        -> Pull Word32 a
        -> Program Block (SPush Block a)
red2 f arr
  | len arr == 1 = return $ push $ arr 
  | otherwise    = 
    do
      let (a1,a2) = halve arr
      arr' <- computePull (zipWith f a1 a2)
      red2 f arr'   

mapRed2 :: Data a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapRed2 f arr = liftGridMap body arr
  where
    body arr = execBlock (red2 f arr)

getRed2 = putStrLn $ 
          genKernel 256 "red2"
            (mapRed2 (+) . splitUp 512 :: DPull EInt32 -> DPush Grid EInt32)


---------------------------------------------------------------------------
-- Kernel3 (Thread acceses element tid and tid+n + last op optimisation
---------------------------------------------------------------------------

red3 :: Data a 
        => Word32 
        -> (a -> a -> a)
        -> Pull Word32 a
        -> Program Block (SPush Block a)
red3 cutoff f  arr
  | len arr == cutoff =
    return $ push $ fold1 f arr
  | otherwise = 
    do
      let (a1,a2) = halve arr
      arr' <- computePull (zipWith f a1 a2)
      red3 cutoff f arr'   


mapRed3 :: Data a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapRed3 f arr = liftGridMap body arr
  where
    body arr = execBlock (red3 2 f arr)

getRed3 = putStrLn $ 
          genKernel 256 "red3"
            (mapRed3 (+) . splitUp 512 :: DPull EInt32 -> DPush Grid EInt32)



---------------------------------------------------------------------------
-- Kernel4 (Thread performs sequential computations)
-- seqReduce is a built-in primitive that results in a for loop.
-- An alternative is to use fold1 and get an unrolled loop in the
-- generated code. 
---------------------------------------------------------------------------
seqReducePush f = singletonPush . seqReduce f 

red4 :: Data a
        => (a -> a -> a)
        -> Pull Word32 a
        -> Program Block (SPush Block a)
red4 f arr =
  do
    arr' <- compute $ liftBlockMap (seqReducePush f) (splitUp 8 arr)
    red3 2 f arr'
    
mapRed4 :: Data a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapRed4 f arr = liftGridMap body arr
  where
    body arr = execBlock (red4 f arr) 

getRed4 = putStrLn $
          genKernel 256 "red4"
            (mapRed4 (+) . splitUp 512 :: DPull EInt32 -> DPush Grid EInt32)


 
---------------------------------------------------------------------------
-- Kernel5 (Thread performs sequential computations, in a coalesced fashion) 
---------------------------------------------------------------------------

red5 :: Data a
        => (a -> a -> a)
        -> Pull Word32 a
        -> Program Block (SPush Block a)
red5 f arr =
  do
    arr' <- compute $ liftBlockMap (seqReducePush f)
                                 (coalesce 8 arr)
    red3 2 f arr' 
  

mapRed5 :: Data a  => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapRed5 f arr = liftGridMap body arr
  where
    body arr = execBlock (red5 f arr) 

getRed5 = putStrLn $
          genKernel 256 "red5"
            (mapRed5 (+) . splitUp 512 :: DPull EInt32 -> DPush Grid EInt32)



---------------------------------------------------------------------------
-- Kernel6 More sequential work 
---------------------------------------------------------------------------

red5' :: Data a 
           => Word32
           -> (a -> a -> a)
           -> Pull Word32 a
           -> Program Block (SPush Block a)
red5' n f arr =
  do
    arr' <- compute $ liftBlockMap (seqReducePush f) (coalesce n arr)
    red3 2 f arr' 

red6 = red5' 16 

mapRed6 :: Data a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapRed6 f arr = liftGridMap body arr
  where
    body arr = execBlock (red6 f arr) 

getRed6 = putStrLn $ 
          genKernel 256 "red6"
            (mapRed6 (+) . splitUp 512 :: DPull EInt32 -> DPush Grid EInt32)



---------------------------------------------------------------------------
-- Kernel7 Even more sequential work 
---------------------------------------------------------------------------

red7 = red5' 32  

mapRed7 :: Data a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapRed7 f arr = liftGridMap body arr
  where
    body arr = execBlock (red7 f arr)

getRed7 = putStrLn $
          genKernel 256 "red7"
            (mapRed7 (+) . splitUp 512 :: DPull EInt32 -> DPush Grid EInt32)




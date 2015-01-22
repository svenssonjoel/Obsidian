{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Reduction where

import Obsidian

import Prelude hiding (zipWith)

import Data.Word

---------------------------------------------------------------------------
-- reduceLocalSilly
---------------------------------------------------------------------------
sumUp :: Pull Word32 EWord32 -> EWord32
sumUp arr 
  | len arr == 1 = arr ! 0 
  | otherwise    =
      let (a1,a2) = halve arr
          arr'    = zipWith (+) a1 a2
      in sumUp arr' 

-- less silly 
sumUp' :: Pull Word32 EWord32 -> Program Block EWord32
sumUp' arr 
  | len arr == 1 = return (arr ! 0)
  | otherwise    = do
      let (a1,a2) = halve arr
      arr' <-  compute (zipWith (+) a1 a2)
      sumUp' arr' 

-- sequential (but expressively so) 
sumUpT :: Pull Word32 EWord32 -> Program Thread EWord32
sumUpT arr 
  | len arr == 1 = return (arr ! 0)
  | otherwise    = do
      let (a1,a2) = halve arr
      arr' <- compute (zipWith (+) a1 a2)
      sumUpT arr' 



mapSumUp :: Pull EWord32 (SPull EWord32) -> Push Grid EWord32 EWord32
mapSumUp arr = asGrid (fmap body arr)
  where
    body a = execBlock' (return (sumUp a)) :: Push Block Word32 EWord32 

mapSumUp' :: Pull EWord32 (SPull EWord32) -> Push Grid EWord32 EWord32
mapSumUp' arr = asGrid (fmap body arr)
  where
    body a = singletonPush (sumUp' a)


-- {{{0,1,2,3,4,5,6,7}},{{8,9,10,11,12,13,14,15}}}
mapSumUpT :: Pull EWord32 (SPull (SPull EWord32)) -> Push Grid EWord32 EWord32
mapSumUpT arr = asGrid (fmap body arr)
  where
    body a = asBlock (fmap bodyThread a) 
    bodyThread a = execThread' (sumUpT a)



---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
reduceLocal :: (t *<=* Block, Compute t, Data a)
               => (a -> a -> a)
               -> SPull a
               -> Program t (SPush t a)
reduceLocal f arr
  | len arr == 1 = return $ push arr
  | otherwise    =
    do
      let (a1,a2) = halve arr
      arr' <- compute $ push $ zipWith f a1 a2
      reduceLocal f arr'


reduceBlock :: forall a. Data a 
          => (a -> a -> a)
          -> SPull a -> Program Block (SPush Block a)
reduceBlock f arr =
  do imm <- compute $ asBlock (fmap body (splitUp 32 arr))
     reduceLocal f imm
  where
    body a = execWarp (reduceLocal f a)

reduceGrid :: forall a. Data a 
          => (a -> a -> a)
          -> DPull a -> DPush Grid a
reduceGrid f arr = asGrid $ fmap body (splitUp 4096 arr) 
    where
      body a = execBlock (reduceBlock f a)


reduce :: Data a 
          => (a -> a -> a)
          -> DPull a -> DPush Grid a
reduce = reduceGrid


input :: DPull EInt32
input = undefinedGlobal (variable "X")

reduceLocal' :: (t *<=* Block, Compute t, Data a)
               => (a -> a -> a)
               -> SPull a
               -> Program t a
reduceLocal' f arr
  | len arr == 1 = return (arr ! 0)
  | otherwise    =
    do
      let (a1,a2) = halve arr
      arr' <- compute $ push $ zipWith f a1 a2
      reduceLocal' f arr'

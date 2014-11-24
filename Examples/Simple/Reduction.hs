{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
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
      arr' <-  forcePull (zipWith (+) a1 a2)
      sumUp' arr' 

-- sequential (but expressively so) 
sumUpT :: Pull Word32 EWord32 -> Program Thread EWord32
sumUpT arr 
  | len arr == 1 = return (arr ! 0)
  | otherwise    = do
      let (a1,a2) = halve arr
      arr' <- forcePull (zipWith (+) a1 a2)
      sumUpT arr' 



mapSumUp :: Pull EWord32 (SPull EWord32) -> Push Grid EWord32 EWord32
mapSumUp arr = asGrid (fmap body arr)
  where
    body a = singletonPush (return (sumUp a)) :: Push Block Word32 EWord32 

mapSumUp' :: Pull EWord32 (SPull EWord32) -> Push Grid EWord32 EWord32
mapSumUp' arr = asGrid (fmap body arr)
  where
    body a = singletonPush (sumUp' a)


-- {{{0,1,2,3,4,5,6,7}},{{8,9,10,11,12,13,14,15}}}
mapSumUpT :: Pull EWord32 (SPull (SPull EWord32)) -> Push Grid EWord32 EWord32
mapSumUpT arr = asGrid (fmap body arr)
  where
    body a = asBlock (fmap bodyThread a) 
    bodyThread a = singletonPush (sumUpT a)



---------------------------------------------------------------------------
--
---------------------------------------------------------------------------

reduceLocal :: (Thread :<=: t, Forceable t, Storable a)
               => (a -> a -> a)
               -> SPull a
               -> Program t (SPush t a)
reduceLocal f arr
  | len arr == 1 = return $ push arr
  | otherwise    =
    do
      let (a1,a2) = halve arr
      arr' <- force $ push $ zipWith f a1 a2
      reduceLocal f arr'

reduceLocal' :: (Thread :<=: t, Forceable t, Storable a)
               => (a -> a -> a)
               -> SPull a
               -> Program t a
reduceLocal' f arr
  | len arr == 1 = return (arr ! 0)
  | otherwise    =
    do
      let (a1,a2) = halve arr
      arr' <- force $ push $ zipWith f a1 a2
      reduceLocal' f arr'

-- local = pJoin

reduceBlocks :: forall a . Storable a
          => (a -> a -> a)
          -> SPull a -> Program Block a
reduceBlocks f arr =
    do
      imm <- compute $ asBlock (fmap body (splitUp 32 arr))
      reduceLocal' f imm
  where
    body a = singletonPush (reduceLocal' f a) :: SPush Warp a 
    
reduceGrid :: forall a. Storable a
          => (a -> a -> a)
          -> DPull a -> DPush Grid a
reduceGrid f arr = asGrid $ fmap body (splitUp 4096 arr) 
    where
      body a = singletonPush (reduceBlocks f a) :: SPush Block a 


reduce :: Storable a
          => (a -> a -> a)
          -> DPull a -> DPush Grid a
reduce = reduceGrid

-- reduce :: MemoryOps a
--           => (a -> a -> a)
--           -> DPull a -> DPush Grid a
-- -- reduce f = pConcatMap $ pJoin . liftM push . reduceLocal f 
-- reduce f arr = pConcat (fmap (reduceLocal f) (splitUp 256 arr))

-- reduceExperiment :: MemoryOps a
--                     => (a -> a -> a)
--                     -> DPull a -> DPush Grid a
-- reduceExperiment f arr =
--     pJoin $ do
--       imm <-  force <- pConcat $ fmap (reduceWarps f) (splitUp 256 arr)
--         -- imm :: SPull (SPush a) 
--       undefined -- pConcat (fmap (reduceLocal f) imm )



input :: DPull EInt32
input = undefinedGlobal (variable "X")


-- getReduce = putStrLn $ genKernel "reduce" (reduce (+) . splitUp 256) (input :- ())


{-# LANGUAGE FlexibleContexts #-} 
module Reduction where

import Obsidian

import Prelude hiding (zipWith)

import Control.Monad

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

sumUp' :: Pull Word32 EWord32 -> Program Block EWord32
sumUp' arr 
  | len arr == 1 = return (arr ! 0)
  | otherwise    = do
      let (a1,a2) = halve arr
      arr' <-  forcePull (zipWith (+) a1 a2)
      sumUp' arr' 

sumUpT :: Pull Word32 EWord32 -> Program Thread EWord32
sumUpT arr 
  | len arr == 1 = return (arr ! 0)
  | otherwise    = do
      let (a1,a2) = halve arr
      arr' <- forcePull (zipWith (+) a1 a2)
      sumUpT arr' 



mapSumUp :: Pull EWord32 (SPull EWord32) -> Push Grid EWord32 EWord32
mapSumUp arr = pConcat (fmap body arr)
  where
    body arr = singletonPush (return (sumUp arr))

mapSumUp' :: Pull EWord32 (SPull EWord32) -> Push Grid EWord32 EWord32
mapSumUp' arr = pConcat (fmap body arr)
  where
    body arr = singletonPush (sumUp' arr)


-- {{{0,1,2,3,4,5,6,7}},{{8,9,10,11,12,13,14,15}}}
mapSumUpT :: Pull EWord32 (SPull (SPull EWord32)) -> Push Grid EWord32 EWord32
mapSumUpT arr = pConcat (fmap body arr)
  where
    body arr = tConcat (fmap bodyThread arr) 
    bodyThread arr = singletonPush (sumUpT arr)



---------------------------------------------------------------------------
--
---------------------------------------------------------------------------

reduceLocal :: (Forceable t, Storable a, Pushable t )
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

reduceLocal' :: (Forceable t, Storable a, Pushable t )
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

reduceBlocks :: Storable a
          => (a -> a -> a)
          -> SPull a -> Program Block a
reduceBlocks f arr =
    do
      imm <- force $ pConcat (fmap body (splitUp 32 arr))
      reduceLocal' f imm
  where
    body arr = singletonPush (reduceLocal' f arr)
    
reduceGrid :: Storable a
          => (a -> a -> a)
          -> DPull a -> DPush Grid a
reduceGrid f arr = pConcat $ fmap body (splitUp 4096 arr) 
    where
      body arr = singletonPush (reduceBlocks f arr)


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


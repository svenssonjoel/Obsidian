{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-} 

module ReduceIntro where

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

------------------------------------------------------------
-- parallel reduction
------------------------------------------------------------

-- generic parallel or sequential reduction
reduce :: (Compute t, Data a)
       => (a -> a -> a)
       -> SPull a
       -> Program t (SPush t a)
reduce f arr
  | len arr == 1 = return $ push arr
  | otherwise    =
    do let (a1,a2) = halve arr
       arr' <- compute $ push $ zipWith f a1 a2
       reduce f arr'

-- 2 stage scan that either 2 level parallel
-- Warp, Block
-- or 1 level sequential and one level parallel
-- Thread, Warp 
reduce2stage :: ( Compute (Step t)
                , Compute t
                , Data a)
             => (a -> a -> a)         
             -> SPull a -> Program (Step t) (SPush (Step t) a)
-- reduce2stage :: (Step t *<=* Block
--                 , Compute (Step t)
--                 , Data a)
--              => (a -> a -> a)         
--              -> SPull a -> Program (Step t) (SPush (Step t) a)
reduce2stage f arr = do
  arr' <- compute $ liftPar (fmap body (splitUp 32 arr))
  reduce f arr' 
  where body a = exec (reduce f a) 

-- create a grid, instatiate reduce2stage as
-- a parallel Warp,Block reduction
reduceGrid :: Data a 
           => (a -> a -> a)
           -> DPull a -> DPush Grid a
reduceGrid f arr = asGridMap body (splitUp 512 arr) 
     where
       body a = execBlock (reduce2stage f a)

reduceGrid1stage :: Data a
            => (a -> a -> a)
            -> DPull a -> DPush Grid a
reduceGrid1stage f arr = asGridMap body (splitUp 512 arr)
  where
    body a = execBlock (reduce f a) 




------------------------------------------------------------
-- Use non-generic combinators for same result
------------------------------------------------------------

reduce2stage' :: Data a
              => (a -> a -> a)         
              -> SPull a -> Program Block (SPush Block a)
reduce2stage' f arr = do
  arr' <- compute $ asBlock (fmap body (splitUp 32 arr))
  reduce f arr' 
  where body a = execWarp (reduce f a) 

reduceGrid' :: Data a 
            => (a -> a -> a)
            -> DPull a -> DPush Grid a
reduceGrid' f arr = asGrid $ fmap body (splitUp 512 arr) 
     where
       body a = execBlock (reduce2stage f a)




---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
-- reduceLocal :: (t *<=* Block, Compute t, Data a)
--                => (a -> a -> a)
--                -> SPull a
--                -> Program t (SPush t a)
-- reduceLocal f arr
--   | len arr == 1 = return $ push arr
--   | otherwise    =
--     do
--       let (a1,a2) = halve arr
--       arr' <- compute $ push $ zipWith f a1 a2
--       reduceLocal f arr'


-- reduceBlock :: forall a. Data a 
--           => (a -> a -> a)
--           -> SPull a -> Program Block (SPush Block a)
-- reduceBlock f arr =
--   do imm <- compute $ asBlock (fmap body (splitUp 32 arr))
--      reduceLocal f imm
--   where
--     body a = execWarp (reduceLocal f a)

-- reduceGrid :: forall a. Data a 
--           => (a -> a -> a)
--           -> DPull a -> DPush Grid a
-- reduceGrid f arr = asGrid $ fmap body (splitUp 4096 arr) 
--     where
--       body a = execBlock (reduceBlock f a)


-- reduce :: Data a 
--           => (a -> a -> a)
--           -> DPull a -> DPush Grid a
-- reduce = reduceGrid


-- input :: DPull EInt32
-- input = undefinedGlobal (variable "X")

-- reduceLocal' :: (t *<=* Block, Compute t, Data a)
--                => (a -> a -> a)
--                -> SPull a
--                -> Program t a
-- reduceLocal' f arr
--   | len arr == 1 = return (arr ! 0)
--   | otherwise    =
--     do
--       let (a1,a2) = halve arr
--       arr' <- compute $ push $ zipWith f a1 a2
--       reduceLocal' f arr'





{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-} 


module SortNet where

import Obsidian

import Prelude hiding (zip, reverse, (<*))
import qualified Prelude as P 

import Data.Word

import Debug.Trace

---------------------------------------------------------------------------
-- basics
---------------------------------------------------------------------------
riffle :: (Data a, ASize s) => Pull s a -> Pull s a 
riffle = unpair . uncurry zip . halve 

riffle' :: (t *<=* Block, Data a, ASize s) => Pull s a -> Push t s (a,a)
riffle' = push . uncurry zip . halve
---------------------------------------------------------------------------
-- compare and swap
---------------------------------------------------------------------------
cmpswap :: (Scalar a, Ord a) => (Exp a,Exp a) -> (Exp a,Exp a) 
cmpswap (a,b) = ifThenElse (b <* a) (b,a) (a,b) 


---------------------------------------------------------------------------
-- Merger
--------------------------------------------------------------------------- 

shex :: (Compute t, Data a)
     => ((a,a) -> (a,a)) -> SPull a -> SPush t a
shex cmp arr =
  exec $ rep (logBaseI 2 (len arr)) (compute . core cmp) arr
  where
    core c = unpairP . push  . fmap c . pair . riffle 

shexRev :: (Compute t, Data a)
     => ((a,a) -> (a,a)) -> SPull a -> SPush t a
shexRev cmp arr =
  let (arr1,arr2) = halve arr
      arr2' = reverse arr2
      arr' = arr1 `append` arr2'
  in 
   exec $ rep (logBaseI 2 (len arr)) (compute . core cmp) arr'
  where
    core c = unpairP . push  . fmap c . pair . riffle 

shexRev' :: (Array (Push t), Compute t, Data a)
         => ((a,a) -> (a,a)) -> SPull a -> SPush t a
shexRev' cmp arr =
  let (arr1,arr2) = halve arr
      arr2' = reverse arr2
      arr' = (push arr1) `append` (push arr2')
  in
   exec $ do
     arr'' <- compute arr' 
     rep (logBaseI 2 (len arr)) (compute . core cmp) arr''
  where
    core c = unpairP . fmap c . riffle'





---------------------------------------------------------------------------
-- Sorter
---------------------------------------------------------------------------
 
{-
  -- | -- | -- | -- 
  -- | -- | -- | --
          |    |
  -- | -- | -- | --
  -- | -- | -- | --
               | 
  -- | -- | -- | --
  -- | -- | -- | --
          |    | 
  -- | -- | -- | --
  -- | -- | -- | --
-}
 
test :: forall a . (Scalar a, Ord a) => SPull (Exp a) -> SPush Block (Exp a) 
test = divConq $ shexRev' cmpswap

mapTest :: (Scalar a, Ord a) => DPull (Exp a) -> DPush Grid (Exp a) 
mapTest arr = asGridMap test (splitUp 1024 arr)


testParam :: forall a . (Scalar a, Ord a) => Word32 -> Word32 -> SPull (Exp a) -> SPush Block (Exp a) 
testParam w b = divConqParam w b  $ shexRev' cmpswap

mapTestParam :: (Scalar a, Ord a) => Word32 -> Word32 -> DPull (Exp a) -> DPush Grid (Exp a) 
mapTestParam w b arr = asGridMap (testParam w b) (splitUp 1024 arr)




-- | divide and conquer skeleton 
divConq :: forall a . Data a
        => (forall  t . (Array (Push t), Compute t) => SPull a -> SPush t a)
        -> SPull  a -> SPush Block  a
divConq f arr = divConqParam w b f arr
  where w = 32  -- hardcoded parameters that may not make sense. 
        b = 256 

-- | Parameterized version of divConq. 
divConqParam :: forall a . Data a
             => Word32
             -> Word32 
             -> (forall  t . (Array (Push t), Compute t) => SPull a -> SPush t a)
             -> SPull  a -> SPush Block  a
divConqParam doWarps doBlocks f arr = execBlock $ doIt (logLen - 1) arr
  where logLen = logBaseI 2 (len arr)
        doIt 0 a = 
          do 
            return  $ (f :: SPull a -> SPush Block a) a

        doIt n a | currLen > doBlocks = blockBody 
                 | currLen > doWarps  = warpBody  
                 | otherwise          = threadBody
          where
            currLen = 2^(logLen - n)
            arrs = splitUp currLen a
            blockBody = 
              do
                arr' <- compute
                        $ asBlockMap (f :: SPull a -> SPush Block a)
                        $ arrs
                doIt (n - 1) arr'
            warpBody =          
              do
                arr' <- compute
                        $ asBlockMap (f :: SPull a -> SPush Warp a)
                        $ arrs
                doIt (n - 1) arr'
            threadBody =
              do
                arr' <- compute
                        $ asBlockMap (f :: SPull a -> SPush Thread a)
                        $ arrs
                doIt (n - 1) arr'



{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{- LANGUAGE UndecidableInstances -} -- remove

{- 2013
   Joel Svensson
   Niklas Ulvinge
-} 

module Obsidian.LibraryG where

import Obsidian.Array
import Obsidian.Program
import Obsidian.Exp
import Obsidian.Memory
import Obsidian.Library

import Control.Monad
import Data.Word

import Prelude hiding (zipWith)

---------------------------------------------------------------------------
-- Parallel concatMap  
---------------------------------------------------------------------------

pConcatMap f = pConcat . fmap f
pUnCoalesceMap f = pUnCoalesce . fmap f
pConcatMapJoin f = pConcat . fmap (pJoin.f)
pUnCoalesceMapJoin f = pUnCoalesce . fmap (pJoin.f)
pCoalesceMap n f = pUnCoalesce . fmap f . coalesce n
pSplitMap n f = pConcat . fmap f . splitUp n

---------------------------------------------------------------------------
-- Parallel Generate 
---------------------------------------------------------------------------

generate :: ASize l
              => l
              -> (EWord32 -> SPush t b)
              -> Push (Step t) l b
generate n f = pConcat $ fmap f $ iterations n 

---------------------------------------------------------------------------
-- Step into the Hierarchy by distributing a
-- Thread program parameterized on a threadId over the threads
-- at a specific level in the Hierarchy. 
---------------------------------------------------------------------------


-- threads :: ASize l
--            => l
--            -> (EWord32 -> SPush Thread b)
--            -> Push t l b  
-- threads n f =
--   mkPush (n * fromIntegral s) $ \wf -> do
--     forAll (sizeConv n) $ \tid -> 
--        let wf' a ix = wf a (tid * sizeConv s + ix)
--            p = f tid 
--        in p <: wf'
--   where
--     s  = len (f (variable "tid")) -- arr

-- | A way to enter into the hierarchy
-- A bunch of Thread computations, spread across the threads of either
-- a Warp, block or grid. (or performed sequentially in a single thread (not implemneted)) 
tConcat :: ASize l
           => Pull l (SPush Thread b)
           -> Push t l b  
tConcat arr =
  mkPush (n * fromIntegral s) $ \wf -> do
    forAll (sizeConv n) $ \tid -> 
       let wf' a ix = wf a (tid * sizeConv s + ix)
           p = arr ! tid -- f tid 
       in p <: wf'
  where
    n = len arr
    s  = len (arr ! 0) --(f (variable "tid")) -- arr

tDistribute :: ASize l
               => l
               -> (EWord32 -> SPush Thread b)
               -> Push t l b
tDistribute n f = tConcat (mkPull n f) 

      
-- | Distribute work across the parallel resources at a given level of the GPU hiearchy
pConcat :: ASize l => Pull l (SPush t a) -> Push (Step t) l a
pConcat arr =
  mkPush (n * fromIntegral rn) $ \wf ->
    distrPar (sizeConv n) $ \bix ->
      let p = arr ! bix 
          wf' a ix = wf a (bix * sizeConv rn + ix) 
          
      in p <: wf'
  where
    n  = len arr
    rn = len (arr ! 0) -- All arrays are same length

-- | Distribute work across the parallel resources at a given level of the GPU hierarchy
pDistribute :: ASize l => l -> (EWord32 -> SPush t a) -> Push (Step t) l a
pDistribute n f = pConcat (mkPull n f) 

-- sequential concatenation of a pull of push 
sConcat :: ASize l => Pull l (SPush t a) -> Push t l a
sConcat arr =
  mkPush (n * fromIntegral rn) $ \wf ->
  do
    seqFor (sizeConv n) $ \bix ->
      let p = arr ! bix -- (Push _ p) = arr ! bix
          wf' a ix = wf a (bix * sizeConv rn + ix)              
      in p <: wf'
  where 
    n  = len arr
    rn = len $ arr ! 0

sDistribute :: ASize l => l -> (EWord32 -> SPush t a) -> Push t l a
sDistribute n f = sConcat (mkPull n f) 

-- pUnCoalesce adapted from Niklas branch.
-- | Combines work that was distributed in a Coalesced way.
-- | Applies a permutation on stores.
pUnCoalesce :: ASize l => Pull l (SPush t a) -> Push (Step t) l a
pUnCoalesce arr =
  mkPush (n * fromIntegral rn) $ \wf ->
  distrPar (sizeConv n) $ \bix ->
    let p = arr ! bix
        wf' a ix = wf a (bix * sizeConv rn + ix)
    in p <: (g wf')
  where
    n  = len arr
    rn = len $ arr ! 0
    s  = sizeConv rn 
    g wf a i = wf a (i `div` s + (i`mod`s)*(sizeConv n))

---------------------------------------------------------------------------
-- load 
---------------------------------------------------------------------------
-- load :: ASize l => Word32 -> Pull l a -> Push Block l a 
-- load n arr =
--   mkPush m $ \wf ->
--   forAll (sizeConv n') $ \tid ->
--   do
--     seqFor (fromIntegral n) $ \ix -> 
--       wf (arr ! (tid + (ix*n'))) (tid + (ix*n')) 

--   where
--     m = len arr
--     n' = sizeConv m `div` fromIntegral n

-- store :: ASize l => Word32 -> Pull l a -> Push Block l a 
-- store = load 



---------------------------------------------------------------------------
-- RunPush 
---------------------------------------------------------------------------

runPush :: Program t (Push t s a) -> Push t s a
runPush = local 



local :: Program t (Push t s a) -> Push t s a 
local = pJoin


local_ :: (a -> Program t (Push t s b)) -> a -> Push t s b 
local_ f = local . f 


localPull :: (ASize s, Pushable t) => Program t (Pull s a) -> Push t s a
localPull = local . liftM push

localPull_ :: (ASize s, Pushable t)
              => (a -> Program t (Pull s b)) -> a -> Push t s b
localPull_ f a = localPull (f a)

pJoin ::  Program t (Push t s a) -> Push t s a
pJoin prg = mkPush n $ \wf -> do
  parr <- prg
  parr <: wf
  -- It is a bit scary that I need to "evaluate" programs here. 
  where n = len $ fst $ runPrg 0 prg

pJoinPush :: (Pushable t, ASize s) => Program t (Pull s a) -> Push t s a
pJoinPush = pJoin . liftM push



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

-- Maybe needs a typeclass 
threads :: ASize l
           => l
           -> (EWord32 -> SPush Thread b)
           -> Push t l b  
threads n f =
  mkPush (n * fromIntegral s) $ \wf -> do
    forAll (sizeConv n) $ \tid -> 
       let wf' a ix = wf a (tid * sizeConv s + ix)
           p = f tid 
       in p <: wf'
  where
    s  = len (f (variable "tid")) -- arr



---------------------------------------------------------------------------
-- Various concatenation
---------------------------------------------------------------------------
-- Simplify the interface here by only allowing concat of (Pull (Push a)) 

-- type family ElementType a
-- type instance ElementType (Pull l a) = a
-- type instance ElementType (Push t l a) = a
-- type instance ElementType (Program t (Push t l a)) = a
-- type instance ElementType (Program t (Pull l a)) = a 
      
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


-- class  Concat p t | p -> t where
--   pConcat ::  ASize l => Pull l (SPush t a) -> Push (Step t) l a -- (ElementType p)

-- instance Concat (Push t Word32 a) t where
--   pConcat = pConcatP . fmap return
 
-- instance Concat (Program t (Push t Word32 a)) t where
--   pConcat prg = pConcatP prg

-- instance Pushable t => Concat (Program t (Pull Word32 a)) t where
--   pConcat prg = pConcatP (fmap (liftM push) prg) 

-- pConcatP :: ASize l => Pull l (Program t (SPush t a)) -> Push (Step t) l a
-- pConcatP arr =
--   mkPush (n * fromIntegral rn) $ \wf ->
--     distrPar (sizeConv n) $ \bix ->
--       let bp = arr ! bix -- (Push _ p) = arr ! bix
--           wf' a ix = wf a (bix * sizeConv rn + ix)
          
--       in do p <- bp 
--             p <: wf'
--   where
--     n  = len arr
--     rn = len $ fst $ runPrg 0 $ core (arr ! 0) 0 -- core hack 

  
-- wConcat :: SPull (SPush Warp a) -> SPush Block a
-- wConcat arr =
--   mkPush (n * fromIntegral rn) $ \wf ->
--      Program $ \ warpid -> -- Here really awkward.
--                  -- I get a warpid from Program, and one from NWarps...
--                  -- And all because Force needs to know this id on the "outside"
--                  -- Nwarps is not really a loop! it should not proive a warpID.
--       NWarps (fromIntegral n) $ \_ -> ---warpID -> 
--         let p = arr ! (variable "warpID")
--             wf' a ix = wf a (variable "warpID" * sizeConv rn + ix)
--         in core (p  <: wf') (variable "warpID")  
--   where
--     n  = len arr
--     rn = len $ (arr ! 0)  -- bit awkward. 
    
         

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
-- Join 
---------------------------------------------------------------------------

-- | A scope for a computation that uses local memory within. The input is a program that
-- uses shared memory to compute some push array. The output is the same push array
-- but all usage of shared memory is hidden.
local :: Program t (Push t s a) -> Push t s a 
local = pJoin

localComp :: (a -> Program t (Push t s b)) -> a -> Push t s b 
localComp f a = local (f a) 

-- | Hides the intermediate results of computing a Push array.
pJoin ::  Program t (Push t s a) -> Push t s a
pJoin prg = mkPush n $ \wf -> Program $ \_ -> do
  parr <- core prg 0
  core (parr <: wf) 0 
  where n = len $ fst $ runPrg 0 (core prg 0) -- core hack  

pJoinPush :: (Pushable t, ASize s) => Program t (Pull s a) -> Push t s a
pJoinPush = pJoin . liftM push


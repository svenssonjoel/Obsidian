{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
                               
{-

   sequential loops with state
   2013 : Joel Svensson 

-}

module Obsidian.SeqLoop where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Array
import Obsidian.Memory
import Obsidian.Names
import Obsidian.Data
import Obsidian.Force
import qualified Obsidian.Library as Lib 

-- TODO: Rename module to something better
--       Or make part of Library.hs
---------------------------------------------------------------------------
-- seqReduce 
---------------------------------------------------------------------------
-- | Sequential reduction of Pull array. Results in a for loop in generated code. 
seqReduce :: Storable a
           => (a -> a -> a)
           -> SPull a
           -> Program Thread a
seqReduce op arr =
  do
    (ns :: Names a)  <- names "v" 
    allocateScalar ns 

    assignScalar ns init  
 
    SeqFor (n-1) $ \ ix ->
      do
        assignScalar ns (readFrom ns `op`  (arr ! (ix + 1)))
    
    return $ readFrom ns
  where
    n = sizeConv$ len arr
    init = arr ! 0 


---------------------------------------------------------------------------
-- Iterate
---------------------------------------------------------------------------
-- | iterate a function. Results in a for loop in generated code. 
seqIterate :: Storable a
              => EWord32
              -> (EWord32 -> a -> a)
              -> a
              -> Program Thread a
seqIterate n f init =
  do
    (ns :: Names a)  <- names "v" 
    allocateScalar ns 

    assignScalar ns init
    SeqFor n $ \ix ->
      do
        assignScalar ns $ f ix (readFrom ns)

    return $ readFrom ns

---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------
-- | iterate a function until a condition holds. Results in a while loop
-- with a break in the generated code.
seqUntil :: Storable a
-- The comment above seems to not desribe reality ! 
            => (a -> a)
            -> (a -> EBool)
            -> a
            -> Program Thread a
seqUntil f p init =
  do 
    (ns :: Names a) <- names "v" 
    allocateScalar ns 

    assignScalar ns init
    SeqWhile (p (readFrom ns)) $ 
      do
        (tmp :: Names a) <- names "t"
        allocateScalar tmp
        assignScalar tmp (readFrom ns) 
        assignScalar ns $ f (readFrom tmp)
    return $ readFrom ns
  
---------------------------------------------------------------------------
-- Sequential scan
---------------------------------------------------------------------------
-- | Sequential scan over the elements in a pull array. Results in a for loop
-- in the generated code. 
seqScan :: Storable a
           => (a -> a -> a)
           -> SPull a
           -> SPush Thread a
seqScan op arr {-(Pull n ixf)-}  =
  mkPush n $ \wf ->  do
    (ns :: Names a) <- names "v" -- (ixf 0) 
    allocateScalar ns -- (ixf 0)
    assignScalar ns (arr ! 0)
    wf (readFrom ns) 0
    SeqFor (sizeConv (n-1)) $ \ix -> do
      assignScalar ns  $ readFrom ns `op` (arr ! (ix + 1))
      wf (readFrom ns) (ix+1)
    where
      n = len arr

-- | Sequential scan that takes a carry-in. 
seqScanCin :: (Storable a, Storable b) 
           => (b -> a -> b)
           -> b -- cin  
           -> SPull a
           -> SPush Thread b
seqScanCin op a arr =
  mkPush n $ \wf ->  do
    (ns :: Names a) <- names "v" 
    allocateScalar ns 
    assignScalar ns a 
    -- wf (readFrom ns) 0 
    SeqFor (sizeConv  n) $ \ix -> do
      assignScalar ns  $ readFrom ns `op` (arr ! ix)
      wf (readFrom ns) ix
  where
    n = len arr
    
-- | Sequential scan with separate types for input, output and accumulator.
mapAccumL :: (ASize s, Storable a, Storable b, Storable acc)
           => (acc -> a -> (acc,b))
           -> acc -- cin
           -> Pull s a
           -> Push Thread s b
mapAccumL op acc arr {-(Pull n ixf)-} =
  mkPush n $ \wf ->  do
    (ns :: Names a) <- names "v" -- (ixf 0)
    allocateScalar ns -- (ixf 0)
    assignScalar ns acc -- (ixf 0)
    -- wf (readFrom ns) 0
    SeqFor (sizeConv  n) $ \ix -> do
      let (newAcc, b) = op (readFrom ns) (arr ! ix)
      -- order of writing matters, because readFrom is evaluated twice
      wf b ix
      assignScalar ns newAcc
  where
    n = len arr

mapAccumR :: (ASize s, Storable a, Storable b, Storable acc)
           => (acc -> a -> (acc,b))
           -> acc -- cin
           -> Pull s a
           -> Push Thread s b
mapAccumR op acc =
   Lib.reverse . mapAccumL op acc . Lib.reverse


--------------------------------------------------------------------------- 
-- sMapAccum
-- Generalisation of the old sConcat functionality.

sMapAccum :: (Compute t, Data acc, ASize l)
             => (acc -> Pull l a -> Program t (acc,Push t l b))
             -> acc
             -> Pull l (Pull l a)
             -> Push t l b
sMapAccum f acc arr =
  
  mkPush (n * fromIntegral rn) $ \wf ->
  do
    (noms :: Names acc) <- names "v"
    --(noms2 :: Names acc) <- names "APA"
    
    allocateSharedScalar noms
   -- allocateScalar noms2
    -- a single thread in the group, performs an assignment
    -- May need synchronization! 
    singleThread $ assignScalar noms acc
    sync
    seqFor (sizeConv n) $ \bix -> do
      --singleThread $ assignScalar noms2 acc 
      (newAcc, b) <- f (readFrom noms) (arr ! bix)
      singleThread $ assignScalar noms newAcc
      sync
      let wf' a ix = wf a (bix * sizeConv rn + ix) 
      b <: wf'
     
  where 
    n  = len arr
    rn = len $ arr ! 0

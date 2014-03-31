{-# LANGUAGE ScopedTypeVariables #-} 
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

import Data.Word

-- TODO: Rename module to something better

---------------------------------------------------------------------------
-- seqReduce (actually reduce) 
---------------------------------------------------------------------------
seqReduce :: (MemoryOps a)
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

-- TODO: This is dangerous when array lengths are unknown! 

---------------------------------------------------------------------------
-- Iterate
---------------------------------------------------------------------------
seqIterate :: (MemoryOps a)
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
seqUntil :: (MemoryOps a) 
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

seqScan :: (MemoryOps a)
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


seqScanCin :: (MemoryOps a)
           => (a -> a -> a)
           -> a -- cin  
           -> SPull a
           -> SPush Thread a
seqScanCin op a arr {-(Pull n ixf)-} =
  mkPush n $ \wf ->  do
    (ns :: Names a) <- names "v" -- (ixf 0) 
    allocateScalar ns -- (ixf 0)
    assignScalar ns a -- (ixf 0)
    -- wf (readFrom ns) 0 
    SeqFor (sizeConv  n) $ \ix -> do
      assignScalar ns  $ readFrom ns `op` (arr ! ix)
      wf (readFrom ns) ix
  where
    n = len arr
---------------------------------------------------------------------------
-- Sequential Map (here for uniformity) 
---------------------------------------------------------------------------

seqMap :: (a -> b)
          -> SPull a
          -> SPush Thread b
seqMap f arr =
  mkPush (len arr) $ \wf -> do
    SeqFor (sizeConv (len arr)) $ \ix ->
      wf (f (arr ! ix)) ix



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
seqReduce :: (ASize l, MemoryOps a)
           => (a -> a -> a)
           -> Pull l a
           -> Push Thread l a
seqReduce op arr =
  mkPush 1 $ \wf -> 
  do
    (ns :: Names a)  <- moNames "v" 
    moAllocateScalar ns 

    moAssignScalar ns init  
 
    seqFor (n-1) $ \ ix ->
      do
        moAssignScalar ns (moReadFrom ns `op`  (arr ! (ix + 1)))
    
    wf (moReadFrom ns) 0 
  where
    n = sizeConv$ len arr
    init = arr ! 0 

-- TODO: This is dangerous when array lengths are unknown! 

---------------------------------------------------------------------------
-- Iterate
---------------------------------------------------------------------------
seqIterate :: (ASize l, MemoryOps a)
              => EWord32
              -> (EWord32 -> a -> a)
              -> a
              -> Push Thread l a
seqIterate n f init =
  mkPush 1 $  \wf -> 
  do
    (ns :: Names a)  <- moNames "v" 
    moAllocateScalar ns 

    moAssignScalar ns init
    seqFor n $ \ix ->
      do
        moAssignScalar ns $ f ix (moReadFrom ns)

    wf (moReadFrom ns) 0 

---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------    
seqUntil :: (ASize l, MemoryOps a) 
            => (a -> a)
            -> (a -> EBool)
            -> a
            -> Push Thread l a
seqUntil f p init =
  mkPush 1 $ \wf -> 
  do 
    (ns :: Names a) <- moNames "v" 
    moAllocateScalar ns 

    moAssignScalar ns init
    SeqWhile (p (moReadFrom ns)) $ 
      do
        (tmp :: Names a) <- moNames "t"
        moAllocateScalar tmp
        moAssignScalar tmp (moReadFrom ns) 
        moAssignScalar ns $ f (moReadFrom tmp)
    wf (moReadFrom ns) 0 
  
---------------------------------------------------------------------------
-- Sequential scan
---------------------------------------------------------------------------

seqScan :: (ASize l, MemoryOps a)
           => (a -> a -> a)
           -> Pull l a
           -> Push Thread l a
seqScan op arr {-(Pull n ixf)-}  =
  mkPush n $ \wf -> do
    (ns :: Names a) <- moNames "v" -- (ixf 0) 
    moAllocateScalar ns -- (ixf 0)
    moAssignScalar ns (arr ! 0)
    wf (moReadFrom ns) 0 
    seqFor (sizeConv (n-1)) $ \ix -> do
      moAssignScalar ns  $ moReadFrom ns `op` (arr ! (ix + 1))
      wf (moReadFrom ns) (ix+1)
    where
      n = len arr


seqScanCin :: (ASize l, MemoryOps a)
           => (a -> a -> a)
           -> a -- cin  
           -> Pull l a
           -> Push Thread l a
seqScanCin op a arr {-(Pull n ixf)-} =
  mkPush n $ \wf -> do
    (ns :: Names a) <- moNames "v" -- (ixf 0) 
    moAllocateScalar ns -- (ixf 0)
    moAssignScalar ns a -- (ixf 0)
    -- wf (readFrom ns) 0 
    seqFor (sizeConv  n) $ \ix -> do
      moAssignScalar ns  $ moReadFrom ns `op` (arr ! ix)
      wf (moReadFrom ns) ix                  
  where
    n = len arr
---------------------------------------------------------------------------
-- Sequential Map (here for uniformity) 
---------------------------------------------------------------------------

seqMap :: ASize l
          => (a -> b)
          -> Pull l a
          -> Push Thread l b
seqMap f arr =
  mkPush (len arr) $ \wf -> do
    seqFor (sizeConv (len arr)) $ \ix ->
      wf (f (arr ! ix)) ix 



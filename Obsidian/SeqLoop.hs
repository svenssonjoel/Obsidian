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
           -> SPush Thread a
seqReduce op arr =
  mkPush 1 $ \wf -> Program $  \id -> 
  do
    (ns :: Names a)  <- moNames "v" 
    moAllocateScalar ns 

    moAssignScalar ns init  
 
    SeqFor (n-1) $ \ ix ->
      do
        moAssignScalar ns (moReadFrom ns `op`  (arr ! (ix + 1)))
    
    core (wf (moReadFrom ns) 0) id
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
              -> SPush Thread a
seqIterate n f init =
  mkPush 1 $  \wf -> Program $ \id ->  
  do
    (ns :: Names a)  <- moNames "v" 
    moAllocateScalar ns 

    moAssignScalar ns init
    SeqFor n $ \ix ->
      do
        moAssignScalar ns $ f ix (moReadFrom ns)

    core (wf (moReadFrom ns) 0) id

---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------    
seqUntil :: (MemoryOps a) 
            => (a -> a)
            -> (a -> EBool)
            -> a
            -> SPush Thread a
seqUntil f p init =
  mkPush 1 $ \wf -> Program $ \id ->  
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
    core (wf (moReadFrom ns) 0) id 
  
---------------------------------------------------------------------------
-- Sequential scan
---------------------------------------------------------------------------

seqScan :: (MemoryOps a)
           => (a -> a -> a)
           -> SPull a
           -> SPush Thread a
seqScan op arr {-(Pull n ixf)-}  =
  mkPush n $ \wf -> Program $ \id -> do
    (ns :: Names a) <- moNames "v" -- (ixf 0) 
    moAllocateScalar ns -- (ixf 0)
    moAssignScalar ns (arr ! 0)
    core (wf (moReadFrom ns) 0) id 
    SeqFor (sizeConv (n-1)) $ \ix -> do
      moAssignScalar ns  $ moReadFrom ns `op` (arr ! (ix + 1))
      core (wf (moReadFrom ns) (ix+1)) id 
    where
      n = len arr


seqScanCin :: (MemoryOps a)
           => (a -> a -> a)
           -> a -- cin  
           -> SPull a
           -> SPush Thread a
seqScanCin op a arr {-(Pull n ixf)-} =
  mkPush n $ \wf -> Program $ \id -> do
    (ns :: Names a) <- moNames "v" -- (ixf 0) 
    moAllocateScalar ns -- (ixf 0)
    moAssignScalar ns a -- (ixf 0)
    -- wf (readFrom ns) 0 
    SeqFor (sizeConv  n) $ \ix -> do
      moAssignScalar ns  $ moReadFrom ns `op` (arr ! ix)
      core (wf (moReadFrom ns) ix) id 
  where
    n = len arr
---------------------------------------------------------------------------
-- Sequential Map (here for uniformity) 
---------------------------------------------------------------------------

seqMap :: (a -> b)
          -> SPull a
          -> SPush Thread b
seqMap f arr =
  mkPush (len arr) $ \wf -> Program $ \id -> do
    SeqFor (sizeConv (len arr)) $ \ix ->
      core (wf (f (arr ! ix)) ix) id 



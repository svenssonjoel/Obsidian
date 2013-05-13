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
-- seqFold (actually reduce) 
---------------------------------------------------------------------------

seqFold :: (ASize l, MemoryOps a)
           => (a -> a -> a)
           -> a
           -> Pull l a
           -> Program Thread a
seqFold op init arr = do
  (ns :: Names a)  <- names "v" -- init 
  allocateScalar ns -- init

  assignScalar ns init  
 
  SeqFor n $ \ ix ->
    do
      assignScalar ns (readFrom ns `op`  (arr ! ix))
    
  return $ readFrom ns
  where 
    n = sizeConv$ len arr

---------------------------------------------------------------------------
-- Iterate
---------------------------------------------------------------------------
seqIterate :: MemoryOps a => EWord32 -> (EWord32 -> a -> a) -> a -> Program Thread a
seqIterate n f init =
  do
    (ns :: Names a)  <- names "v" -- init
    allocateScalar ns -- init

    assignScalar ns init
    SeqFor n $ \ix ->
      do
        assignScalar ns $ f ix (readFrom ns)

    return $ readFrom ns

---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------    
seqUntil :: MemoryOps a
                 => (a -> a)
                 -> (a -> EBool)
                 -> a
                 -> Program Thread a
seqUntil f p init =
  do 
    (ns :: Names a) <- names "v" -- init
    allocateScalar ns -- init

    assignScalar ns init
    SeqWhile (p (readFrom ns)) $ 
      do
        (tmp :: Names a) <- names "t"
        allocateScalar tmp
        assignScalar tmp (readFrom ns) 
        assignScalar ns $ f (readFrom tmp)
    
    return $ readFrom ns


seqUntil' :: MemoryOps a
                 => (a -> a)
                 -> (a -> EBool)
                 -> a
                 -> SPush Thread a
seqUntil' f p init =
  Push 1 $ \wf -> 
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
    wf (readFrom ns) 0 
  
---------------------------------------------------------------------------
-- Sequential scan
---------------------------------------------------------------------------

seqScan :: (ASize l, MemoryOps a)
           => (a -> a -> a)
           -> Pull l a
           -> Push Thread l a
seqScan op (Pull n ixf)  =
  Push n $ \wf -> do
    (ns :: Names a) <- names "v" -- (ixf 0) 
    allocateScalar ns -- (ixf 0)
    assignScalar ns (ixf 0)
    wf (readFrom ns) 0 
    SeqFor (sizeConv (n-1)) $ \ix -> do
      wf (readFrom ns) ix                  
      assignScalar ns  $ readFrom ns `op` (ixf (ix + 1))
     
                 
---------------------------------------------------------------------------
-- Sequential Map (here for uniformity) 
---------------------------------------------------------------------------

seqMap :: forall l a b. ASize l
          => (a -> b)
          -> Pull l a
          -> Push Thread l b
seqMap f arr =
  Push (len arr) $ \wf -> do
    SeqFor (sizeConv (len arr)) $ \ix ->
      wf (f (arr ! ix)) ix 



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
  ns  <- names "v" init 
  allocateScalar ns init

  assignScalar ns init  
 
  SeqFor n $ \ ix ->
    do
      assignScalar ns (readFrom ns `op`  (arr ! ix))
      return None

    
  return $ readFrom ns
  where 
    n = sizeConv$ len arr

---------------------------------------------------------------------------
-- Iterate
---------------------------------------------------------------------------
seqIterate :: MemoryOps a => EWord32 -> (a -> a) -> a -> Program Thread a
seqIterate n f init =
  do
    ns <- names "v" (init)
    allocateScalar ns (init)

    assignScalar ns init
    SeqFor n $ \ix ->
      do
        assignScalar ns $ f (readFrom ns)
        return None

    return $ readFrom ns

---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------
-- | iterate a function until some condition holds or the maximum number
--   of iterations is reached
seqUntilBound :: MemoryOps a
                 => EWord32
                 -> (a -> a)
                 -> (a -> EBool)
                 -> a
                 -> Program Thread a
seqUntilBound n f p init =
  do 
    ns <- names "v" (init)
    allocateScalar ns (init)

    assignScalar ns init
    SeqFor n $ \ix ->
      do
        assignScalar ns $ f (readFrom ns)
        Cond (p (readFrom ns)) Break
        return None

    return $ readFrom ns

---------------------------------------------------------------------------
-- Sequential scan
---------------------------------------------------------------------------

seqScan :: (ASize l, MemoryOps a)
           => (a -> a -> a)
           -> Pull l a
           -> Push Thread l a
seqScan op (Pull n ixf)  =
  Push n $ \wf -> do
    ns <- names "v" (ixf 0) 
    allocateScalar ns (ixf 0)
    assignScalar ns (ixf 0)
    wf (readFrom ns) 0 
    SeqFor (sizeConv (n-1)) $ \ix -> do
      wf (readFrom ns) ix                  
      assignScalar ns  $ readFrom ns `op` (ixf (ix + 1))
      return None
                 
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


---------------------------------------------------------------------------
-- Sequential Map and scan (generalisation of map + accum) 
---------------------------------------------------------------------------
{- 
seqMapScan :: forall l a b acc. (ASize l, MemoryOps acc, MemoryOps b)
              => (acc -> a -> (acc,b))
              -> acc 
              -> Pull l a
              -> Push Thread l (acc,b)
seqMapScan op acc (Pull n ixf)  =
  Push n $ \wf -> do
    ns <- names "v" (undefined :: b) 
    allocateScalar ns (undefined :: b)
    nacc <- names "v" (undefined :: acc)
    allocateScalar nacc (undefined :: acc)
    
    assignScalar nacc acc

    SeqFor (sizeConv n) $ \ix -> do
      let (a,b) = op (readFrom nacc) (ixf ix)
      wf (a,b) ix                  
      assignScalar nacc a
      return None
-} 
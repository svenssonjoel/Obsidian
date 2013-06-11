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
  Push 1 $ \wf -> 
  do
    (ns :: Names a)  <- names "v" 
    allocateScalar ns 

    assignScalar ns init  
 
    SeqFor (n-1) $ \ ix ->
      do
        assignScalar ns (readFrom ns `op`  (arr ! (ix + 1)))
    
    wf (readFrom ns) 0 
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
  Push 1 $  \wf -> 
  do
    (ns :: Names a)  <- names "v" 
    allocateScalar ns 

    assignScalar ns init
    SeqFor n $ \ix ->
      do
        assignScalar ns $ f ix (readFrom ns)

    wf (readFrom ns) 0 

---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------    
-- seqUntil :: MemoryOps a
--                  => (a -> a)
--                  -> (a -> EBool)
--                  -> a
--                  -> Program Thread a
-- seqUntil f p init =
--   do 
--     (ns :: Names a) <- names "v" 
--     allocateScalar ns 

--     assignScalar ns init
--     SeqWhile (p (readFrom ns)) $ 
--       do
--         (tmp :: Names a) <- names "t"
--         allocateScalar tmp
--         assignScalar tmp (readFrom ns) 
--         assignScalar ns $ f (readFrom tmp)
    
--     return $ readFrom ns


seqUntil :: (ASize l, MemoryOps a) 
            => (a -> a)
            -> (a -> EBool)
            -> a
            -> Push Thread l a
seqUntil f p init =
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
      assignScalar ns  $ readFrom ns `op` (ixf (ix + 1))
      wf (readFrom ns) (ix+1)



seqScanCin :: (ASize l, MemoryOps a)
           => (a -> a -> a)
           -> a -- cin  
           -> Pull l a
           -> Push Thread l a
seqScanCin op a (Pull n ixf) =
  Push n $ \wf -> do
    (ns :: Names a) <- names "v" -- (ixf 0) 
    allocateScalar ns -- (ixf 0)
    assignScalar ns a -- (ixf 0)
    -- wf (readFrom ns) 0 
    SeqFor (sizeConv  n) $ \ix -> do
      assignScalar ns  $ readFrom ns `op` (ixf ix)
      wf (readFrom ns) ix                  
     
                 
---------------------------------------------------------------------------
-- Sequential Map (here for uniformity) 
---------------------------------------------------------------------------

seqMap :: ASize l
          => (a -> b)
          -> Pull l a
          -> Push Thread l b
seqMap f arr =
  Push (len arr) $ \wf -> do
    SeqFor (sizeConv (len arr)) $ \ix ->
      wf (f (arr ! ix)) ix 



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
seqScanCin :: Storable a
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
    

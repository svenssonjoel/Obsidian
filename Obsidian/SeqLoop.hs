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

-- TODO: Add suitable allocs
-- TODO: Rename module to something better
-- TODO: Generalise all operations somewhat.
--       (similar to the StoreOps class in Force)
-- TODO: Sketch on a Lift class
--       lift local to global
--       lift thread to block
--       etc!
--       (This is more general than this module, belongs somewhere else!) 

---------------------------------------------------------------------------
-- seqFold (actually reduce) 
---------------------------------------------------------------------------

seqFold :: forall l a. (ASize l, MemoryOps a)
           => (a -> a -> a)
           -> a
           -> Pull l a
           -> Program Thread a
seqFold op init arr = do
  ns  <- names (undefined :: a) 
  allocateScalar ns (undefined :: a)

  assignScalar ns init  
  -- Assign nom [] init  
  SeqFor n $ (\ ix ->
      assignScalar ns (readFrom ns `op`  (arr ! ix))) 

    
  return $ readFrom ns
  where 
    n = sizeConv$ len arr


---------------------------------------------------------------------------
-- Sequential scan
---------------------------------------------------------------------------

seqScan :: forall l a. (ASize l, MemoryOps a)
           => (a -> a -> a)
           -> Pull l a
           -> Push Thread l a
seqScan op (Pull n ixf)  =
  Push n $ \wf -> do
    ns <- names (undefined :: a) 
    allocateScalar ns (undefined :: a)
    assignScalar ns (ixf 0)
    wf (readFrom ns) 0 
    SeqFor (sizeConv (n-1)) $ \ix -> do
      wf (readFrom ns) ix                  
      assignScalar ns  $ readFrom ns `op` (ixf (ix + 1))
                 


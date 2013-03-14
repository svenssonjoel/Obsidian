{-# LANGUAGE ScopedTypeVariables #-} 
{-

   sequential loops with state
   2013 : Joel Svensson 

-}

module Obsidian.SeqLoop where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Array 

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
-- Hacking, No real plan
---------------------------------------------------------------------------

seqFold :: (ASize l, Scalar a)
           => (Exp a -> Exp a -> Exp a)
           -> (Exp a)
           -> Pull l (Exp a)
           -> Program Thread (Exp a)
seqFold op init arr = do
  nom <- allocateLS init 
  Assign nom [] init  
  SeqFor n $ (\ ix ->
      Assign nom [] (variable nom `op`  (arr ! ix)))

    
  return $ variable nom
  where 
    n = sizeConv$ len arr


---------------------------------------------------------------------------
-- Hacking, No real plan
---------------------------------------------------------------------------

seqScan :: (ASize l, Scalar a)
           => (Exp a -> Exp a -> Exp a)
           -> Pull l (Exp a)
           -> Push Thread l (Exp a)
seqScan op (Pull n ixf)  =
  Push n $ \wf -> do
    i <- Identifier  -- allocateLS (undefined :: a)
    let nom = "v" ++ show i
    Assign nom [] (ixf 0)
    wf (variable nom) 0 
    SeqFor (sizeConv (n-1)) $ \ix -> do
      wf (variable nom) ix                  
      Assign nom [] $ variable nom `op` (ixf (ix +1))
                 


{-# LANGUAGE FlexibleInstances,      -- Can we live without these ? 
             UndecidableInstances,   -- 
             ScopedTypeVariables #-}

module Obsidian.Lift where

import Obsidian.Program
import Obsidian.Exp 
import Obsidian.Array
import Obsidian.Force 

import Data.Word
-- Start sketching on lift functions.
-- TODO: Think about how to deal with Push arrays


class LiftT a where
  liftT :: Pull Word32 (TProgram a) -> BProgram (Pull Word32 a)   
  

class LiftB a where
  liftB :: ASize l => (Pull l (BProgram a)) -> GProgram (Pull l a)
  


---------------------------------------------------------------------------
-- instances LiftT 
---------------------------------------------------------------------------  

instance Pushable p => LiftT (p Word32 a) where
  liftT = error "liftT: not implemented for this type" 

instance StoreOps a => LiftT a where
  liftT arr@(Pull ts txf) =
    do
      snames <- names (undefined :: a)
      allocate snames (undefined :: a) ts
      let p wf = do
            forAll (fromIntegral ts) $ \tix -> 
              do
                elts <- txf tix
                wf elts tix 
      p (assign snames)
      return $ pullFrom snames ts
      



---------------------------------------------------------------------------
-- instances LiftB 
---------------------------------------------------------------------------  

instance LiftB a where
  liftB = error "liftB: is not implemented"

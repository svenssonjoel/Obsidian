{-# LANGUAGE FlexibleInstances,      -- Can we live without these ? 
             UndecidableInstances,   --
             OverlappingInstances,   -- 
             ScopedTypeVariables,
             MultiParamTypeClasses,
             TypeFamilies #-}

module Obsidian.Lift where

import Obsidian.Program
import Obsidian.Exp 
import Obsidian.Array
import Obsidian.Force
import Obsidian.Memory
import Obsidian.Names 

import Data.Word
-- Start sketching on lift functions.
-- TODO: Think about how to deal with Push arrays


---------------------------------------------------------------------------
-- testing
---------------------------------------------------------------------------


class LiftB a where
  liftB :: Pull Word32 (TProgram a) -> BProgram (Pull Word32 a)

class LiftG a where
  type Elt a
  liftG :: ASize l => Pull l (BProgram a) -> GProgram (Push Grid l (Elt a)) 

---------------------------------------------------------------------------
-- instances LiftB 
---------------------------------------------------------------------------  

instance Pushable p => LiftB (p Word32 a) where
  liftB = error "liftB: not implemented for this type" 

instance MemoryOps a => LiftB a where
  liftB arr@(Pull ts txf) =
    do
      --snames <- names "arr" (undefined :: a)
      --allocateArray snames (undefined :: a) ts
      let p wf = do
            forAll (fromIntegral ts) $ \tix -> 
              do
                elts <- txf tix
                wf elts tix 
      snames <- p (assignArrayN ts)
      return $ pullFrom snames ts
      

--class PromoteToBlock a where
--  promoteToBlock :: Pull Word32 (TProgram a) -> BProgram Word32 a
  
      
        


---------------------------------------------------------------------------
-- instances LiftB 
---------------------------------------------------------------------------  
-- This one has bugs ! 
-- Each block computes an array
instance MemoryOps a => LiftG (Pull Word32 a) where
  type Elt (Pull Word32 a) = a 
  liftG (Pull bs bxf) =
    do
      let tmp = fst $ runPrg 0 (bxf 0) -- get info about result  

      --snames <- names "arr" (undefined :: a)
      --allocateArray snames (undefined :: a) (len tmp)
      
      snames <- forAllBlocks (sizeConv bs) $ \bix -> do 
        arr <- bxf bix
        let (Push _ p) = push Block arr
        p (assignArrayN (len tmp)) 

      let pully = Pull bs $ \bix -> pullFrom snames (len tmp) 

      return $ Push (bs * (fromIntegral (len tmp))) $
        \wf ->
        do
          forAllBlocks (sizeConv bs) $ \bix ->
            forAll (sizeConv (len tmp)) $ \tix ->
              do
                wf ((pully ! bix) ! tix) (bix * (sizeConv (len tmp)) + tix)
               
                    
          

    

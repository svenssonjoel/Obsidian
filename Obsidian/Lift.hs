{-# LANGUAGE FlexibleInstances,      -- Can we live without these ? 
             UndecidableInstances,   --
             OverlappingInstances,   -- 
             ScopedTypeVariables,
             TypeFamilies #-}

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
  type Elt a
  liftB :: ASize l => Pull l (BProgram a) -> GProgram (Push Grid l (Elt a)) 


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

-- Each block computes an array
instance StoreOps a => LiftB (Pull Word32 a) where
  type Elt (Pull Word32 a) = a 
  liftB (Pull bs bxf) =
    do
      let tmp = fst $ runPrg 0 (bxf 0) -- get info about result  

      snames <- names (undefined :: a)
      allocate snames (undefined :: a) (len tmp)
      
      forAllBlocks (sizeConv bs) $ \bix -> do 
        arr <- bxf bix
        let (Push _ p) = push Block arr
        p (assign snames) 

      let pully = Pull bs $ \bix -> pullFrom snames (len tmp) 

      return $ Push (bs * (fromIntegral (len tmp))) $
        \wf ->
        do
          forAllBlocks (sizeConv bs) $ \bix ->
            forAll (sizeConv (len tmp)) $ \tix ->
              wf ((pully ! bix) ! tix) (bix * (sizeConv (len tmp)) + tix)
                    
          
-- Each block computes a value
--instance  StoreOps a => LiftB a where
--  liftB = error "liftB: is not implemented for this type" 

    

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
import Control.Monad


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
      return $ pullFromS snames ts -- mkPullArray ts $ \ix -> readFrom snames
      

--class PromoteToBlock a where
--  promoteToBlock :: Pull Word32 (TProgram a) -> BProgram Word32 a
  
      
        


---------------------------------------------------------------------------
-- instances LiftB 
---------------------------------------------------------------------------  

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

      let pully = Pull bs $ \bix -> pullFromS snames (len tmp) 

      return $ Push (bs * (fromIntegral (len tmp))) $
        \wf ->
        do
          forAllBlocks (sizeConv bs) $ \bix ->
            forAll (sizeConv (len tmp)) $ \tix ->
              do
                wf ((pully ! bix) ! tix) (bix * (sizeConv (len tmp)) + tix)
               
                    
          
{- 
pushBlocks :: ASize l => Pull l (BProgram (Pull Word32 a)) -> Push Grid l a     
pushBlocks arr =
  Push s $ \wf -> 
    do
      forAllBlocks (sizeConv (len arr)) $
       \ bix -> 
        do
          sm <- arr ! bix
          forAll (sizeConv (len sm)) $
            \ tix ->
              wf (sm ! tix) (bix * (sizeConv (len sm)) + tix)

  where
    s = len arr * (fromIntegral s')
    s' = len $ fst $ runPrg 0 (arr ! 0)
-} 
--shBlocks :: (Pushable p, ASize l) => Pull l (BProgram (p Word32 a)) -> Push Grid l a     
--pushBlocks arr = pushBlocks' $ fmap (liftM (push Block)) arr 


pushBlocks :: (Array p, Pushable p, ASize l) => Pull l (BProgram (p Word32 a)) -> Push Grid l a     
pushBlocks arr =
  Push s $ \wf -> 
    do
      forAllBlocks (sizeConv (len arr)) $
       \ bix -> 
        do
          sm <- arr ! bix
          let (Push m p) = push Block sm
          let wf' a ix = wf a (bix * sizeConv m + ix)
          p wf'
  where
    s = len arr * (fromIntegral s')
    s' = len $ fst $ runPrg 0 (arr ! 0)
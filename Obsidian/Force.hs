
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
----------------------------------------
{- LANGUAGE KindSignatures -}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-} 


{- Joel Svensson 2012..2015

   Notes:
   2014-03-28: Changed API.
               Not using Obsidian.Mutable currently, it needs more work. 
   2013-06-24: Changed code. uses Obsidian.Mutable now
   2013-05-02: Removing things to do with forceG
               Removed the extensions (no longer needed) 
   2013-04-27: Something is broken. 
   2013-04-10: Looking at force and threads
   2013-01-27: globArrays nolonger exist
   2013-01-02: Added simple forceG for globArrays
   2012-12-10: Edited 

-}
 


module Obsidian.Force ( unsafeWritePush
                      , unsafeWritePull
                      , compute_
                      , computePull_
                      , ComputeAs(..)
                      , Compute) where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Array
import Obsidian.Memory 

import Obsidian.Names
import Obsidian.Data

import Data.Word


---------------------------------------------------------------------------
--
-------------------------------------------------------------------------

-- | Compute constraint. 
type Compute t = (Write t, t *<=* Block)

-- | Arrays can be computed at level t if level t allows compute.
class Compute t => ComputeAs t a where
  compute :: Data e => a Word32 e -> Program t (Pull Word32 e)
  
instance Compute t => ComputeAs t Pull where
  compute = computePull_ 

{- 
   The key to this instance is that when matching up instances the
   -whatever matcher that does that-
   matches only against the head, ignoring the constraint.
   meaning that all variations of t, t1 is caught by this
   instance. Though, those where t and t1 are not equal
   a type error is the result (rather than a missing instance).

   This means that the constraint "Compute Block (Push Thread)"
   matches this instance, but is a type error.
-} 
instance (t ~ t1, Compute t) => ComputeAs t (Push t1) where
  compute =  compute_

compute_ :: (Data a, Compute t)
          => Push t Word32 a -> Program t (Pull Word32 a)      
compute_ arr = do
  rval <- unsafeWritePush False arr
  sync
  return rval

computePull_ :: (t *<=* Block, Data a, Compute t)
             => Pull Word32 a -> Program t (Pull Word32 a)  
computePull_ arr = 
  if (len arr <= 32)
  then do
    rval <- unsafeWritePush True parr
    return rval
  else do
    rval <- unsafeWritePush False parr 
    sync
    return rval
  where parr = push arr

               
---------------------------------------------------------------------------
-- Force local (requires static lengths!)
---------------------------------------------------------------------------

class Write t where
  unsafeWritePush :: Storable a => Bool -> Push t Word32 a -> Program t (Pull Word32 a)

-- What to do about volatile here?
-- Ignoring that parameter for now.
-- Thought: It does not matter.
-- Thought: Is this function correct at all?
--   What happens if a thread program allocates memory
-- DONE: The above problem has been fixed! 
instance  Write Thread where
  unsafeWritePush _ p =
    do
      (snames :: Names a)  <- names "arr" 

      -- Here I know that this pattern match will succeed
      let n = len p
    
      allocateArray snames  n
      p <: threadAssignArray snames (variable "tid") n  
      
      return $ threadPullFrom snames (variable "tid") n

instance  Write Warp where
  unsafeWritePush _ p  =
    do
      let n = len p
      noms <- names "arr"
      allocateVolatileArray noms n
     
      p <: warpAssignArray noms (variable "warpID") n 
      return $ warpPullFrom noms (variable "warpID") n

instance Write Block where
  unsafeWritePush volatile p =
    do
      let  n = len p
      noms <- names "arr"
      if (volatile)
        then allocateVolatileArray noms n
        else allocateArray noms n
             
      p <: assignArray noms 
      return $ pullFrom noms n



---------------------------------------------------------------------------
-- unsafe!
---------------------------------------------------------------------------
unsafeWritePull :: (t *<=* Block, Write t, Storable a) => Bool -> Pull Word32 a -> Program t (Pull Word32 a)
unsafeWritePull t = unsafeWritePush t . push





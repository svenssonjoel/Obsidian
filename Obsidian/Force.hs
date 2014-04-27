{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}



{- Joel Svensson 2012, 2013 

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

module Obsidian.Force (Forceable, force, forcePull, unsafeForce) where
-- Write, force, forcePull, unsafeForce, unsafeWritePush) where 


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Array
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Memory 

import Obsidian.Names

import Obsidian.Dimension

import Data.Word

import Control.Monad
---------------------------------------------------------------------------
-- Force local (requires static lengths!)
---------------------------------------------------------------------------

class Write t where
  unsafeWritePush :: Storable a => Bool -> Push t Static d a -> Program t (Pull Static d a)
  -- unsafeWritePull :: MemoryOps a => Pull Word32 a -> Program t (Pull Word32 a) 
  
instance Write Warp where
  unsafeWritePush volatile p  =
    do
      let n = len p
      names <- names "arr"
      allocateVolatileArray names n
     
      p <: warpAssignArray names (variable "warpID") n 
      return $ warpPullFrom names (variable "warpID") n

instance Write Block where
  unsafeWritePush volatile p =
    do
      let  n = len p
      names <- names "arr"
      if (volatile)
        then allocateVolatileArray names n
        else allocateArray names n
             
      p <: assignArray names 
      return $ pullFrom names n

instance Write Thread where
  unsafeWritePush volatile p =
    do
      (snames :: Names a)  <- names "arr" 

      -- Here I know that this pattern match will succeed
      let n = len p
    
      allocateArray snames  n
      p <: assignArray snames 
      
      return $ pullFrom snames n

---------------------------------------------------------------------------
-- Force functions 
---------------------------------------------------------------------------

-- | It is possible to force at level T if we can Write and Sync at that level. 
class (Sync t, Write t) => Forceable t

instance Forceable Thread
instance Forceable Warp
instance Forceable Block


-- | force turns a @Push@ array to a @Program@ generating a @Pull@ array.
--   The returned array represents reading from an array manifest in memory.
force :: (Storable a, Forceable t)
         => Push t Static d a -> Program t (Pull Static d a)      
force arr = do
  rval <- unsafeWritePush False arr
  sync
  return rval

-- | Make a @Pull@ array manifest in memory. 
forcePull :: (Storable a, Forceable t) 
             => Pull Static d a -> Program t (Pull Static d a)  
forcePull = unsafeForce . push 

-- | unsafeForce is dangerous on @Push@ arrays as it does not
--   insert synchronization primitives. 
unsafeForce :: (Storable a, Forceable t) =>
         Push t Static d a -> Program t (Pull Static d a)      
unsafeForce arr = 
  if (len arr <= 32)
  then do
    rval <- unsafeWritePush True arr
    return rval
  else do
    rval <- unsafeWritePush False arr 
    sync
    return rval 






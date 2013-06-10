{-# LANGUAGE ScopedTypeVariables,
             FlexibleInstances #-}


{- Joel Svensson 2012, 2013 

   Notes: 
   2013-05-02: Removing things to do with forceG
               Removed the extensions (no longer needed) 
   2013-04-27: Something is broken. 
   2013-04-10: Looking at force and threads
   2013-01-27: globArrays nolonger exist
   2013-01-02: Added simple forceG for globArrays
   2012-12-10: Edited 

-}

module Obsidian.Force (force,unsafeForce,unsafeWrite) where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Array
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Memory

import Obsidian.Names

import Data.Word
---------------------------------------------------------------------------
-- Force local (requires static lengths!) 
---------------------------------------------------------------------------

class Array p => Write p where
  unsafeWrite :: MemoryOps a => p Word32 a -> BProgram (Pull Word32 a)

instance Write Pull where
  unsafeWrite arr = do 
    (snames :: Names a)  <- names "arr" 

    -- Here I know that this pattern match will succeed
    let n = len arr
      
    allocateArray snames  n

    let (Push m p) = push arr

    p (assignArray snames) 
      
    return $ pullFrom snames n

instance Write (Push Block) where
  unsafeWrite (Push m p) = do 
    (snames :: Names a)  <- names "arr" 

    allocateArray snames  m

    p (assignArray snames) 
      
    return $ pullFrom snames m

instance Write (Push Thread) where
  unsafeWrite (Push m p) = do 
    (snames :: Names a)  <- names "arr" 

    allocateArray snames  m

    forAll 1 $ \_ ->         --One thread
      p (assignArray snames) 
      
    return $ pullFrom snames m

  
force :: (Write p, MemoryOps a) =>  p Word32 a -> BProgram (Pull Word32 a)
force arr = do
  rval <- unsafeWrite arr
  Sync
  return rval

-- Is there an issue with force and Push arrays ?
--  # Threads can write at other locations than thread id!
--  # what does pullFrom do ? (does it make sense!)
-- We are in some sense assuming well-behaved push arrays here !
--  # can we force a 32 element push array without syncing?


unsafeForce :: MemoryOps a => SPull a -> BProgram (SPull a) 
unsafeForce arr | len arr < 32 = unsafeWrite arr 
unsafeForce arr = force arr

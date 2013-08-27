{-# LANGUAGE ScopedTypeVariables,
             FlexibleInstances #-}


{- Joel Svensson 2012, 2013 

   Notes:
   2013-06-24: Changed code. uses Obsidian.Mutable now
   2013-05-02: Removing things to do with forceG
               Removed the extensions (no longer needed) 
   2013-04-27: Something is broken. 
   2013-04-10: Looking at force and threads
   2013-01-27: globArrays nolonger exist
   2013-01-02: Added simple forceG for globArrays
   2012-12-10: Edited 

-}

module Obsidian.Force (force,unsafeForce,unsafeWrite, forceScalar, forceWarp) where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Array
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Memory 

import Obsidian.Names

import qualified Obsidian.Mutable as M

import Data.Word
---------------------------------------------------------------------------
-- Force local (requires static lengths!)
-- A higher level interface over (forceTo, writeTo) 
---------------------------------------------------------------------------

class Array p => Write p where
  unsafeWrite :: MemoryOps a => p Word32 a -> BProgram (Pull Word32 a)

instance Write Pull where
  unsafeWrite arr = 
    do
      (mut :: M.Mutable M.Shared a) <- M.newS parr 
      return $ M.pullFrom mut
      
   where parr = push arr 

instance Write (Push Block) where
  unsafeWrite arr  = 
    do
      (mut :: M.Mutable M.Shared a) <- M.newS arr
      return $ M.pullFrom mut

-- Still not using the Mutable arrays.. problematic 
instance Write (Push Thread) where
  unsafeWrite p = do 
    (snames :: Names a)  <- moNames "arr" 

    -- Here I know that this pattern match will succeed
    let n = len p
      
    moAllocateArray snames  n

--     let p = push arr
    forAll 1 $ \_ ->
      p <: moAssignArray snames
      
    return $ moPullFrom snames n


  
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
unsafeForce arr | len arr <= 32 = unsafeWrite arr 
unsafeForce arr = force arr


---------------------------------------------------------------------------
-- Force a scalar
---------------------------------------------------------------------------

forceScalar :: MemoryOps a => a -> TProgram a
forceScalar a =
  do
    names <- moNames "s"
    moAllocateScalar names
    moAssignScalar names a
    return $ moReadFrom names
    
---------------------------------------------------------------------------
-- Force in a warp program
---------------------------------------------------------------------------
forceWarp :: MemoryOps a => SPush Warp a -> WProgram (SPull a) 
forceWarp p =
  WProgram $ \warpID -> 
   do          
    --let p = push arr
    let n = len p
    names <- moNames "arr"
    moAllocateArray names n
    p <: (moWarpAssignArray names warpID n) 
    return $ moWarpPullFrom names warpID n
   

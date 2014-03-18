{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}



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

module Obsidian.Force (Write, force, forcePull, unsafeForce, unsafeWritePush) where 


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Array
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Memory 

import Obsidian.Names

import qualified Obsidian.Mutable as M

import Data.Word

import Control.Monad
---------------------------------------------------------------------------
-- Force local (requires static lengths!)
-- A higher level interface over (forceTo, writeTo) 
---------------------------------------------------------------------------

class Write t where
  unsafeWritePush :: MemoryOps a => Push t Word32 a -> Program t (Pull Word32 a)
  -- unsafeWritePull :: MemoryOps a => Pull Word32 a -> Program t (Pull Word32 a) 
  
instance Write Warp where
  unsafeWritePush arr  =
    do
      let p = arr
      let n = len p
      names <- moNames "arr"
      moAllocateArray names n
      -- These monads need to be sorted out. What operation goes in what Monad ?
      -- Here the Program \_ is a Thread program, So It should really have a nothing argument
      p <: moWarpAssignArray names (variable "warpID") n 
      return $ moWarpPullFrom names (variable "warpID") n

instance Write Block where
  unsafeWritePush arr =
    do
      let p = arr
      (mut :: M.Mutable M.Shared a) <- M.newS p
      return $ M.pullFrom mut

instance Write Thread where
  unsafeWritePush arr =
    do
      let p = arr
      (snames :: Names a)  <- moNames "arr" 

      -- Here I know that this pattern match will succeed
      let n = len p
    
      moAllocateArray snames  n
      p <: moAssignArray snames 
      
      return $ moPullFrom snames n


force :: (MemoryOps a, Sync t, Write t)
         => Push t Word32 a -> Program t (Pull Word32 a)      
force arr = do
  rval <- unsafeWritePush arr
  sync
  return rval

forcePull :: (MemoryOps a, Sync t, Write t, Pushable t) 
             => Pull Word32 a -> Program t (Pull Word32 a)  
forcePull = unsafeForce . push 

unsafeForce :: (MemoryOps a, Sync t, Write t) =>
         Push t  Word32 a -> Program t (Pull Word32 a)      
unsafeForce arr = do
  rval <- unsafeWritePush arr
  when (len arr > 32) sync
  return rval









-- class Write p where
--   type HLevel p 
  
--   unsafeWrite :: MemoryOps a => Push (HLevel p) Word32 a -> p (Pull Word32 a)


-- instance Write WProgram where
--   type HLevel WProgram = Warp

--   unsafeWrite p =
--     WProgram $ \warpID -> 
--     do 
--       let n = len p
--       names <- moNames "arr"
--       moAllocateArray names n
--       p <: (moWarpAssignArray names warpID n) 
--       return $ moWarpPullFrom names warpID n

-- instance Write (Program Block) where
--   type HLevel (Program Block) = Block 
--   unsafeWrite p =
--     do
--       (mut :: M.Mutable M.Shared a) <- M.newS p
--       return $ M.pullFrom mut

-- instance Write (Program Thread) where
--   type HLevel (Program Thread) = Thread

--   unsafeWrite p =
--     do 
--       (snames :: Names a)  <- moNames "arr" 

--       -- Here I know that this pattern match will succeed
--       let n = len p
      
--       moAllocateArray snames  n

--       p <: moAssignArray snames
      
--       return $ moPullFrom snames n
  
--forceP :: (Sync p, Write p, MemoryOps a) =>  Push (HLevel p) Word32 a -> p (Pull Word32 a)
--forceP arr = do
--  rval <- unsafeWrite arr
--  sync
--  return rval


--force :: (Sync p, Write p, MemoryOps a, Pushable (HLevel p))
--             => Pull Word32 a -> p (Pull Word32 a) 
--force = forceP . push 

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------

-- class Array p => Write p where
--   unsafeWrite :: MemoryOps a => p Word32 a -> BProgram (Pull Word32 a)

-- instance Write Pull where
--   unsafeWrite arr = 
--     do
--       (mut :: M.Mutable M.Shared a) <- M.newS parr 
--       return $ M.pullFrom mut
      
--    where parr = push arr 

-- instance Write (Push Block) where
--   unsafeWrite arr  = 
--     do
--       (mut :: M.Mutable M.Shared a) <- M.newS arr
--       return $ M.pullFrom mut

-- -- Still not using the Mutable arrays.. problematic 
-- instance Write (Push Thread) where
--   unsafeWrite p = do 
--     (snames :: Names a)  <- moNames "arr" 

--     -- Here I know that this pattern match will succeed
--     let n = len p
      
--     moAllocateArray snames  n

-- --     let p = push arr
--     forAll 1 $ \_ ->
--       p <: moAssignArray snames
      
--     return $ moPullFrom snames n


  
-- force :: (Write p, MemoryOps a) =>  p Word32 a -> BProgram (Pull Word32 a)
-- force arr = do
--   rval <- unsafeWrite arr
--   Sync
--   return rval

-- -- Is there an issue with force and Push arrays ?
-- --  # Threads can write at other locations than thread id!
-- --  # what does pullFrom do ? (does it make sense!)
-- -- We are in some sense assuming well-behaved push arrays here !
-- --  # can we force a 32 element push array without syncing?


-- unsafeForce :: MemoryOps a => SPull a -> BProgram (SPull a) 
-- unsafeForce arr | len arr <= 32 = unsafeWrite arr 
-- unsafeForce arr = force arr


-- ---------------------------------------------------------------------------
-- -- Force a scalar
-- ---------------------------------------------------------------------------

-- forceScalar :: MemoryOps a => a -> TProgram a
-- forceScalar a =
--   do
--     names <- moNames "s"
--     moAllocateScalar names
--     moAssignScalar names a
--     return $ moReadFrom names
    
-- ---------------------------------------------------------------------------
-- -- Force in a warp program
-- ---------------------------------------------------------------------------
-- forceWarp :: MemoryOps a => SPush Warp a -> WProgram (SPull a) 
-- forceWarp p =
--   WProgram $ \warpID -> 
--    do          
--     --let p = push arr
--     let n = len p
--     names <- moNames "arr"
--     moAllocateArray names n
--     p <: (moWarpAssignArray names warpID n) 
--     return $ moWarpPullFrom names warpID n
   

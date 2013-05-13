{-# LANGUAGE ScopedTypeVariables #-}


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

--  write should be internal use only
module Obsidian.Force (force,write) where


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


write :: forall a p. (Array p, Pushable p, MemoryOps a) => p Word32 a -> BProgram (Pull Word32 a)
write arr = do 
  (snames :: Names a)  <- names "arr" --(undefined :: a)

  -- Here I know that this pattern match will succeed
  let n = len arr
  
  allocateArray snames {-(undefined :: a)-} n

  let (Push m p) = push Block arr

  p (assignArray snames) 
      
  return $ pullFrom snames n

  
force :: (Array p, Pushable p, MemoryOps a) =>  p Word32 a -> BProgram (Pull Word32 a)
force arr = do
  rval <- write arr
  Sync
  return rval


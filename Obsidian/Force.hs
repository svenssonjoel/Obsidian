
{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             ScopedTypeVariables,
             TypeFamilies,
             GADTs  #-}

{- Joel Svensson 2012, 2013 

   Notes:
   2013-04-27: Something is broken. 
   2013-04-10: Looking at force and threads
   2013-01-27: globArrays nolonger exist
   2013-01-02: Added simple forceG for globArrays
   2012-12-10: Edited 

-}

--  write_ should be internal use only
module Obsidian.Force (write,
                       force,
                       forceG
                      ) where


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

--class Forceable arr where
--  write :: arr Word32 a -> BProgram (Pull Word32 a)
--  force 


write :: (Array p, Pushable p, MemoryOps a) => p Word32 a -> BProgram (Pull Word32 a)
write arr = do 
  -- snames <- names "arr" (undefined :: a)

  -- Here I know that this pattern match will succeed
  let n = len arr
  
  -- allocateArray snames (undefined :: a) n

  let (Push m p) = push Block arr

  snames <- p (assignArrayN n) 
      
  return $ pullFromS snames n

  
force :: (Array p, Pushable p, MemoryOps a) =>  p Word32 a -> BProgram (Pull Word32 a)
force arr = do
  rval <- write arr
  Sync
  return rval


forceG :: GlobalMemoryOps a
        => Push Grid l a
        -> GProgram () 
forceG (Push _ p)  =
  do
    -- output <- outputs (undefined :: a) -- <- this line 
   
    p (\a e -> do {assignOut a e; return (Single (Var,""))}) 
    return ()

{-# LANGUAGE ScopedTypeVariables #-}

{- Joel Svensson 2013 -}

module Obsidian.Mutable (Mutable, new)  where 



import Obsidian.Exp
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Program
import qualified  Obsidian.Memory as Mem
import Obsidian.Names
import Obsidian.Array

import Data.Word
---------------------------------------------------------------------------
-- Mutable arrays 
---------------------------------------------------------------------------

--data Shared
--data Global

-- Starting with implementing only the shared mem kind
data Mutable a = Mutable Word32 (Names a) 

---------------------------------------------------------------------------
-- Create Mutable arrays
---------------------------------------------------------------------------

new :: Mem.MemoryOps a => Word32 -> BProgram (Mutable a)
new n = do
  (snames :: Names a) <- Mem.names "arr"
  Mem.allocateArray snames n
  return $ Mutable n snames 




---------------------------------------------------------------------------
-- forceTo
---------------------------------------------------------------------------
forceTo :: Mem.MemoryOps a => Mutable a -> Pull Word32 a -> BProgram ()
forceTo (Mutable n snames) arr | n <= m =
  do
    p (Mem.assignArray snames)
                               | otherwise = error "forceTo: Incompatible sizes" 
  where
    (Push _ p) = push arr
    m = len arr

-- Add forceTo with offsets (why? just thought it might be useful) 

---------------------------------------------------------------------------
-- pullFrom 
---------------------------------------------------------------------------

pullFrom :: Mem.MemoryOps a => Mutable a -> SPull a
pullFrom (Mutable n snames) = Mem.pullFrom snames n  


{-# LANGUAGE ScopedTypeVariables #-}

{- Joel Svensson 2013 -}

module Obsidian.Mutable ( Mutable
                        , new
                        , forceTo
                        , writeTo
                        , pullFrom )  where 



import Obsidian.Exp
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Program
import qualified  Obsidian.Memory as Mem
import Obsidian.Names
import Obsidian.Array
import Obsidian.Atomic 

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
-- forceTo & writeTo
---------------------------------------------------------------------------
writeTo :: Mem.MemoryOps a => Mutable a -> Pull Word32 a -> BProgram ()
writeTo (Mutable n snames) arr | n <= m =
  do
    p (Mem.assignArray snames)
                               | otherwise = error "forceTo: Incompatible sizes" 
  where
    (Push _ p) = push arr
    m = len arr

-- Add forceTo with offsets (why? just thought it might be useful)
forceTo :: Mem.MemoryOps a => Mutable a -> Pull Word32 a -> BProgram ()
forceTo = writeTo
---------------------------------------------------------------------------
-- pullFrom 
---------------------------------------------------------------------------

pullFrom :: Mem.MemoryOps a => Mutable a -> SPull a
pullFrom (Mutable n snames) = Mem.pullFrom snames n  


---------------------------------------------------------------------------
-- Atomics
---------------------------------------------------------------------------

atomicInc :: Mutable EWord32 -> BProgram ()
atomicInc (Mutable n noms) = mapNamesM_ f noms
    where
      f nom =
        forAll (fromIntegral n) $ \tid -> 
          AtomicOp nom tid AtomicInc  >> return ()


-- Also exists for float and long long (Not implemented) 
atomicAdd :: SPull EWord32 -> Mutable EWord32 -> BProgram ()
atomicAdd arr (Mutable n noms) | m <= n = mapNamesM_ f noms
                               | otherwise = error "atomicAdd: incompatible sizes" 
  where
    f nom =
      forAll (fromIntegral m) $ \tid ->
        AtomicOp nom tid (AtomicAdd (arr ! tid)) >> return ()
    m = len arr    
  
    
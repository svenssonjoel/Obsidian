{-# LANGUAGE ScopedTypeVariables #-}

{- Joel Svensson 2013 -}

module Obsidian.Mutable ( Mutable
                        , Shared
                        , Global 
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
data Shared
data Global

-- Starting with implementing only the shared mem kind
data Mutable s a = Mutable Word32 (Names a) 

---------------------------------------------------------------------------
-- Create Mutable Shared memory arrays
---------------------------------------------------------------------------

new :: Mem.MemoryOps a => Word32 -> BProgram (Mutable Shared a)
new n = do
  (snames :: Names a) <- Mem.names "arr"
  Mem.allocateArray snames n
  return $ Mutable n snames 




---------------------------------------------------------------------------
-- forceTo & writeTo
---------------------------------------------------------------------------
writeTo :: Mem.MemoryOps a => Mutable Shared a -> Push Block Word32 a -> BProgram ()
writeTo (Mutable n snames) (Push m p) 
  | n <= m = p (Mem.assignArray snames)
  | otherwise = error "forceTo: Incompatible sizes" 
  
-- Add forceTo with offsets (why? just thought it might be useful)
forceTo :: Mem.MemoryOps a => Mutable Shared a -> Push Block Word32 a -> BProgram ()
forceTo m arr =
  do
    writeTo m arr
    Sync 
---------------------------------------------------------------------------
-- pullFrom 
---------------------------------------------------------------------------

pullFrom :: Mem.MemoryOps a => Mutable Shared a -> SPull a
pullFrom (Mutable n snames) = Mem.pullFrom snames n  


---------------------------------------------------------------------------
-- Atomics
---------------------------------------------------------------------------

atomicInc :: Mutable s EWord32 -> BProgram ()
atomicInc (Mutable n noms) = mapNamesM_ f noms
    where
      f nom =
        forAll (fromIntegral n) $ \tid -> 
          AtomicOp nom tid AtomicInc  >> return ()


-- Also exists for float and long long (Not implemented) 
atomicAdd :: SPull EWord32 -> Mutable s EWord32 -> BProgram ()
atomicAdd arr (Mutable n noms) | m <= n = mapNamesM_ f noms
                               | otherwise = error "atomicAdd: incompatible sizes" 
  where
    f nom =
      forAll (fromIntegral m) $ \tid ->
        AtomicOp nom tid (AtomicAdd (arr ! tid)) >> return ()
    m = len arr    
  
    

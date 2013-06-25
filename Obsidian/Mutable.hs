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

{-
  Todo: Think about Global vs Shared.
  Todo: Add creation of mutable global arrays. 
 
-} 

---------------------------------------------------------------------------
-- Mutable arrays 
---------------------------------------------------------------------------
data Shared
data Global

-- Starting with implementing only the shared mem kind
data Mutable s a = Mutable Word32 (Names a) 

type MShared a = Mutable Shared a
type MGlobal a = Mutable Global a 
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
writeTo :: Mem.MemoryOps a
           => Mutable Shared a
           -> Push Block Word32 a
           -> BProgram ()
writeTo (Mutable n snames) (Push m p) 
  | n <= m = p (Mem.assignArray snames)
  | otherwise = error "forceTo: Incompatible sizes" 
  
-- Add forceTo with offsets (why? just thought it might be useful)
forceTo :: Mem.MemoryOps a
           => Mutable Shared a
           -> Push Block Word32 a
           -> BProgram ()
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
{-
  How would one implement large histogram using this ?

  -- Need to be able to spread the atomicOp over many blocks.
     Preferably using some slightly general construct. 
  -- 

-} 
-- | Increment atomically 
atomicInc :: forall a s. AtomicInc a
             => Mutable s (Exp a)
             -> EWord32 -- Block size | 
             -> EWord32 -- Block Id   | - a shape. 
             -> BProgram ()
atomicInc (Mutable n noms) bs bid = mapNamesM_ f noms
  where
    f nom =
      forAll (fromIntegral n) $ \tid -> 
      AtomicOp nom (bid * bs + tid) (AtomicInc  :: Atomic a) >> return ()
  

-- | Add atomically 
atomicAdd :: forall a s. AtomicAdd a => SPull (Exp a) -> Mutable s (Exp a) -> BProgram ()
atomicAdd arr (Mutable n noms) | m <= n = mapNamesM_ f noms
                               | otherwise = error "atomicAdd: incompatible sizes" 
  where
    f nom =
      forAll (fromIntegral m) $ \tid ->
        AtomicOp nom tid (AtomicAdd (arr ! tid)) >> return ()
    m = len arr    

-- | Subtract atomically     
atomicSub :: forall a s. AtomicSub a => SPull (Exp a) -> Mutable s (Exp a) -> BProgram ()
atomicSub arr (Mutable n noms) | m <= n = mapNamesM_ f noms
                               | otherwise = error "atomicAdd: incompatible sizes" 
  where
    f nom =
      forAll (fromIntegral m) $ \tid ->
        AtomicOp nom tid (AtomicSub (arr ! tid)) >> return ()
    m = len arr     


atomicExch :: forall a s. AtomicSub a
              => SPull (Exp a)
              -> Mutable s (Exp a)
              -> BProgram (SPull (Exp a))
atomicExch arr (Mutable n noms) | m <= n = undefined 
                                | otherwise = error "atomicAdd: incompatible sizes" 
  
  where
    m = len arr    

{-

---------------------------------------------------------------------------
atomicExch()

int atomicExch(int* address, int val);
unsigned int atomicExch(unsigned int* address,
                        unsigned int val);
unsigned long long int atomicExch(unsigned long long int* address,
                                  unsigned long long int val);
float atomicExch(float* address, float val);

---------------------------------------------------------------------------
atomicMin()

int atomicMin(int* address, int val);
unsigned int atomicMin(unsigned int* address,
                       unsigned int val);
unsigned long long int atomicMin(unsigned long long int* address,
                                 unsigned long long int val);

---------------------------------------------------------------------------
atomicMax()

int atomicMax(int* address, int val);
unsigned int atomicMax(unsigned int* address,
                       unsigned int val);
unsigned long long int atomicMax(unsigned long long int* address,
                                 unsigned long long int val);


---------------------------------------------------------------------------
atomicInc()

unsigned int atomicInc(unsigned int* address,
                       unsigned int val);

---------------------------------------------------------------------------
atomicDec()

unsigned int atomicDec(unsigned int* address,
                       unsigned int val);

---------------------------------------------------------------------------
atomicCAS()

int atomicCAS(int* address, int compare, int val);
unsigned int atomicCAS(unsigned int* address,
                       unsigned int compare,
                       unsigned int val);
unsigned long long int atomicCAS(unsigned long long int* address,
                                 unsigned long long int compare,
                                 unsigned long long int val);

---------------------------------------------------------------------------
atomicAnd()

int atomicAnd(int* address, int val);
unsigned int atomicAnd(unsigned int* address,
                       unsigned int val);
unsigned long long int atomicAnd(unsigned long long int* address,
                                 unsigned long long int val);

---------------------------------------------------------------------------
atomicOr()

int atomicOr(int* address, int val);
unsigned int atomicOr(unsigned int* address,
                      unsigned int val);
unsigned long long int atomicOr(unsigned long long int* address,
                                unsigned long long int val);
---------------------------------------------------------------------------
atomicXor()

int atomicXor(int* address, int val);
unsigned int atomicXor(unsigned int* address,
                       unsigned int val);
unsigned long long int atomicXor(unsigned long long int* address,
                                 unsigned long long int val);

-} 

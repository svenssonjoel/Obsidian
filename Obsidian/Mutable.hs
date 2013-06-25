{-# LANGUAGE ScopedTypeVariables #-}

{- Joel Svensson 2013 -}

module Obsidian.Mutable ( Mutable(Mutable) 
                        , Shared
                        , Global 
                        , newS
                        , forceTo
                        , writeTo
                        , pullFrom
                        , atomicInc
                        , mutlen -- hack
                        )  where 



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

  Todo: Make mutable interface (atomic ops) very low-level 
-} 

---------------------------------------------------------------------------
-- Mutable arrays 
---------------------------------------------------------------------------
data Shared
data Global

-- Starting with implementing only the shared mem kind
data Mutable s a = Mutable Word32 (Names a)

mutlen (Mutable n _) = n

type MShared a = Mutable Shared a
type MGlobal a = Mutable Global a 
---------------------------------------------------------------------------
-- Create Mutable Shared memory arrays
---------------------------------------------------------------------------

newS :: Mem.MemoryOps a => SPush Block a -> BProgram (Mutable Shared a)
newS arr = do
  (snames :: Names a) <- Mem.names "arr"
  Mem.allocateArray snames n
  let mut = Mutable n snames
  forceTo mut arr
  return $ mut -- Mutable n snames 
  where
    n = len arr



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

pullFrom :: Mem.MemoryOps a => Mutable s a -> SPull  a
pullFrom (Mutable n snames) = Mem.pullFrom snames n  


---------------------------------------------------------------------------
-- Atomics
---------------------------------------------------------------------------
-- | Increment atomically 
atomicInc :: forall a s t . AtomicInc a
             => EWord32  
             -> Mutable s (Exp a)
             -> TProgram ()
atomicInc ix (Mutable n noms) = mapNamesM_ f noms
  where
    f nom = AtomicOp nom ix (AtomicInc  :: Atomic a) >> return ()
  

-- | Add atomically 
atomicAdd :: forall a s. AtomicAdd a
             => EWord32
             -> Exp a 
             -> Mutable Shared (Exp a)
             -> TProgram ()
atomicAdd ix v (Mutable n noms) = mapNamesM_ f noms
  where
    f nom = AtomicOp nom ix (AtomicAdd v) >> return ()
  

-- | Subtract atomically     
atomicSub :: forall a s. AtomicSub a
             => EWord32
             -> Exp a
             -> Mutable s (Exp a)
             -> TProgram ()
atomicSub ix v (Mutable n noms) = mapNamesM_ f noms
                               
  where
    f nom = AtomicOp nom ix (AtomicSub v) >> return ()


-- Special case ? No. 
atomicExch :: forall a s. AtomicExch a
              => EWord32
              -> Exp a
              -> Mutable s (Exp a)
              -> TProgram (Exp a)
atomicExch ix v  (Mutable n (Single nom)) = f nom
  where
    f nom = AtomicOp nom ix (AtomicExch v) 
                         
  
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

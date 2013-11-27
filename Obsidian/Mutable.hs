{-# LANGUAGE ScopedTypeVariables,
             TypeFamilies #-}

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
                        , namedMutable
                        , undefinedMutable
                        )  where 



import Obsidian.Exp
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Program
import Obsidian.Memory
import Obsidian.Names
import Obsidian.Array
import Obsidian.Atomic 

import Data.Word

{-
  Todo: Think about Global vs Shared.
  Todo: Add creation of mutable global arrays. 

  Todo: Make mutable interface (atomic ops) very low-level


  TODO: Rethink. Have two sepparate types of mutable arrays.
        Also Skip the Type family magic if possible.
        Make both kinds of Mutable arrays an instance of Array 
-} 

---------------------------------------------------------------------------
-- Mutable arrays 
---------------------------------------------------------------------------
data Shared
data Global

-- EXPERIMENTS 
-- Memory hierarchy size correspondence   
type family MSize a
type instance MSize Shared = Word32
type instance MSize Global = EWord32

-- Memory hierarchy program correspondence
type family MProgram a
type instance MProgram Shared = Block
type instance MProgram Global = Grid 
  
-- Starting with implementing only the shared mem kind
data Mutable s a = Mutable (MSize s) (Names a)

mutlen (Mutable n _) = n

type MShared a = Mutable Shared a
type MGlobal a = Mutable Global a


namedMutable s v = Mutable v (Single s)
undefinedMutable v = Mutable v undefined 
---------------------------------------------------------------------------
-- Create Mutable Shared memory arrays
--   # allocates shared memory
---------------------------------------------------------------------------

newS :: MemoryOps a => SPush Block a -> Prog Block (Mutable Shared a)
newS arr = do
  (snames :: Names a) <- moNames "arr"
  moAllocateArray snames n
  let mut = Mutable n snames
  writeTo mut arr
  return $ mut -- Mutable n snames 
  where
    n = len arr

---------------------------------------------------------------------------
-- forceTo & writeTo
---------------------------------------------------------------------------
writeTo :: MemoryOps a
           => Mutable Shared a
           -> Push Block Word32 a
           -> Prog Block ()
writeTo (Mutable n snames) p 
  | n <= m = p <: (moAssignArray snames)
  | otherwise = error "forceTo: Incompatible sizes" 
  where
    m = len p
    
-- Add forceTo with offsets (why? just thought it might be useful)
forceTo :: MemoryOps a
           => Mutable Shared a
           -> Push Block Word32 a
           -> Prog Block ()
forceTo m arr =
  do
    writeTo m arr
    sync 
---------------------------------------------------------------------------
-- pullFrom 
---------------------------------------------------------------------------

pullFrom :: MemoryOps a => Mutable Shared a -> Prog Block (SPull a)
pullFrom (Mutable n snames) = moPullFrom snames n  


---------------------------------------------------------------------------
-- Atomics
---------------------------------------------------------------------------
-- | Increment atomically 
atomicInc :: forall a s t . AtomicInc a
             => EWord32  
             -> Mutable s (Exp a)
             -> Prog Thread ()
atomicInc ix (Mutable n noms) = mapNamesM_ f noms
  where
    f nom = Prog $ \_ ->  AtomicOp nom ix (AtomicInc  :: Atomic a) >> return ()
  

-- | Add atomically 
atomicAdd :: forall a s. AtomicAdd a
             => EWord32
             -> Exp a 
             -> Mutable Shared (Exp a)
             -> TProgram ()
atomicAdd ix v (Mutable n noms) = mapNamesM_ f noms
  where
    f nom = Prog $ \_ -> AtomicOp nom ix (AtomicAdd v) >> return ()
  

-- | Subtract atomically     
atomicSub :: forall a s. AtomicSub a
             => EWord32
             -> Exp a
             -> Mutable s (Exp a)
             -> TProgram ()
atomicSub ix v (Mutable n noms) = mapNamesM_ f noms
                               
  where
    f nom = Prog $ \_ -> AtomicOp nom ix (AtomicSub v) >> return ()


-- Special case ? No. 
atomicExch :: forall a s. AtomicExch a
              => EWord32
              -> Exp a
              -> Mutable s (Exp a)
              -> TProgram (Exp a)
atomicExch ix v  (Mutable n (Single nom)) = f nom
  where
    f nom = Prog $ \_ -> AtomicOp nom ix (AtomicExch v) 
                         
  
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

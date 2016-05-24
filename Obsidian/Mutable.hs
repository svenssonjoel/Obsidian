{-# LANGUAGE ScopedTypeVariables,
             TypeFamilies,
             EmptyDataDecls,
             FlexibleInstances #-}

{- Joel Svensson 2013, 2014 -}

module Obsidian.Mutable ( Mutable(Mutable) 
                        , Shared
                        , Global
                        , MShared
                        , MGlobal
                        , newSharedMutable
                        , writeToSync
                        , writeTo
                        , assignMutable
                        , indexMutable 
                        , mutableToPull
                        , atomicInc
                        )  where 



import Obsidian.Exp
import Obsidian.Program
import Obsidian.Memory
import Obsidian.Names
import Obsidian.Array
import Obsidian.Atomic

import Data.Word

{-
  notes: 2016       : The mutable arrays will have a bigger role 
                      in version 0.5.0.0 
 
 
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
--
-- Global mutable arrays can only be passed as inputs to a function. 
-- Shared mutable arrays may be created using newS 
--

data Shared 
data Global
data Private 
  
-- A mutable array has an attached location.
-- Either it recides in Global, Shared or private memory. 
data Mutable mloc s a = Mutable s (Names a)

type MShared a = Mutable Shared Word32 a
type MGlobal a = Mutable Global EWord32 a

instance ArrayLength (Mutable Shared) where
  len (Mutable n _) = n

  
---------------------------------------------------------------------------
-- Create Mutable Shared memory arrays
--   # allocates shared memory
---------------------------------------------------------------------------

-- | Create a new Mutable array in shared memory 
newSharedMutable :: Storable a => SPush Block a -> Program Block (Mutable Shared Word32 a)
newSharedMutable arr = do
  (snames :: Names a) <- names "arr"
  allocateArray snames n
  let mut = Mutable n snames
  writeTo mut arr
  return $ mut -- Mutable n snames 
  where
    n = len arr

---------------------------------------------------------------------------
-- forceTo & writeTo
---------------------------------------------------------------------------
-- Much Hacking here

-- | Write a Push array into a mutable array.
--   There is no synchronisation inserted after the write
writeTo :: Storable a
           => Mutable Shared Word32 a
           -> Push Block Word32 a
           -> Program Block ()
writeTo (Mutable n snames) p 
  | n <= m =  p <: assignArray snames
  | otherwise = error "WriteTo: Incompatible sizes" 
  where
    m = len p
   
    
-- Add forceTo with offsets (why? just thought it might be useful)
-- | Write  a Push array into a mutable array and sync
writeToSync :: Storable a
               => Mutable Shared Word32 a
               -> Push Block Word32 a
               -> Program Block ()
writeToSync m arr =
  do
    writeTo m arr
    Sync

---------------------------------------------------------------------------
-- Low level operations
---------------------------------------------------------------------------

-- | Write a value into a storable array. 
assignMutable :: Storable a => Mutable loc l a -> EWord32 ->  a -> Program Thread ()
assignMutable  (Mutable _ snames) ix a = assignArray snames a ix 
 
indexMutable :: (ASize s, Storable a) => Mutable loc s a -> EWord32 -> a 
indexMutable mut ix = mutableToPull mut ! ix 
    
---------------------------------------------------------------------------
-- mutable to pull conversion
---------------------------------------------------------------------------

-- | Convert a Mutable array to a Pull array 
mutableToPull :: (ASize s, Storable a) => Mutable l s a -> Pull s a
mutableToPull (Mutable n snames) = pullFrom snames n  



---------------------------------------------------------------------------
-- Atomics
---------------------------------------------------------------------------
-- | Increment atomically 
atomicInc :: forall mloc a s t . AtomicInc a
             => EWord32  
             -> Mutable mloc s (Exp a)
             -> TProgram ()
atomicInc ix (Mutable n noms) = mapNamesM_ f noms
  where
    f nom = atomicOp nom ix (AtomicInc  :: Atomic a) >> return ()
  

-- | Add atomically 
atomicAdd :: forall mloc a s. AtomicAdd a
             => EWord32
             -> Exp a 
             -> Mutable mloc s  (Exp a)
             -> TProgram ()
atomicAdd ix v (Mutable n noms) = mapNamesM_ f noms
  where
    f nom = atomicOp nom ix (AtomicAdd v) >> return ()
  

-- | Subtract atomically     
atomicSub :: forall mloc a s. AtomicSub a
             => EWord32
             -> Exp a
             -> Mutable mloc s (Exp a)
             -> TProgram ()
atomicSub ix v (Mutable n noms) = mapNamesM_ f noms
                               
  where
    f nom = atomicOp nom ix (AtomicSub v) >> return ()


-- Special case ? No. 
atomicExch :: forall mloc a s. AtomicExch a
              => EWord32
              -> Exp a
              -> Mutable mloc s (Exp a)
              -> TProgram ()
atomicExch ix v  (Mutable n (Single nom)) = f nom
  where
    f nom = atomicOp nom ix (AtomicExch v) 
                         
  
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

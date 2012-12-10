{-# LANGUAGE FlexibleInstances, 
             FlexibleContexts,
             MultiParamTypeClasses,
             TypeOperators,
             TypeFamilies ,
             CPP #-}

{- Joel Svensson 2012 -} 
module Obsidian.CodeGen.InOut where 

import Obsidian.Exp 
import Obsidian.Array

import Obsidian.Types
import Obsidian.Globs 
import Obsidian.Program
import Obsidian.ModifyArray
import qualified Obsidian.CodeGen.Program as CG 

import Data.Word
import Data.Int
      
---------------------------------------------------------------------------
-- New approach (hopefully)
---------------------------------------------------------------------------
-- "reify" Haskell functions into CG.Programs

{-
   Blocks needs to be of specific sizes (a design choice we've made).
   Because of this a prototypical input array needs to be provided
   that has a static block size (the number of blocks is dynamic).

   To make things somewhat general a heterogeneous list of input arrays
   that has same shape as the actual parameter list of the function
   is passed into toProgram (the reifyer). 

-} 
  
type Inputs = [(Name,Type)] 

class ToProgram a b where
  toProgram :: Int -> (a -> b) -> Ips a b -> (Inputs,CG.Program ())

#define toprgBase(t) \
instance ToProgram (Distrib (Array Pull (Exp t))) (Program a) where { \
  toProgram i f (Distrib n blkf)  =      \
    ([(nom,Pointer t)],CG.runPrg (f input)) \
     where {nom = "input" ++ show i; \
            var = "N" ++ show i; \
            n   = len (blkf (variable "X")); \
            input = namedGlobal  nom (variable var) n;}}  \
;\
instance ToProgram (Distrib (Array Pull (Exp t))) (Final (Program a)) where { \
  toProgram i f (Distrib n blkf)  =      \
    ([(nom,Pointer t)],CG.runPrg (cheat (f input))) \
     where  {nom = "input" ++ show i; \
            var = "N" ++ show i; \
            n   = len (blkf (variable "X")); \
            input = namedGlobal  nom (variable var) n;}} 

toprgBase(Int)

toprgBase(Int8)
toprgBase(Int16)
toprgBase(Int32)
toprgBase(Int64)

toprgBase(Word)
toprgBase(Word8)
toprgBase(Word16)
toprgBase(Word32)
toprgBase(Word64)

toprgBase(Float)
toprgBase(Double) 

#define toprgRec(t) \
instance ToProgram b c => ToProgram (Distrib (Array Pull (Exp t))) (b -> c) where{\
  toProgram i f ((Distrib n blkf) :-> rest) = ((nom,Pointer t):ins,prg)\
    where {\
      (ins,prg) = toProgram (i+1) (f input) rest;\
      nom = "input" ++ show i;\
      var = "N" ++ show i;\
      n   = len (blkf (variable "X"));\
      input = namedGlobal  nom (variable var) n;}}\

toprgRec(Int)

toprgRec(Int8)
toprgRec(Int16)
toprgRec(Int32)
toprgRec(Int64)

toprgRec(Word)
toprgRec(Word8)
toprgRec(Word16)
toprgRec(Word32)
toprgRec(Word64)

toprgRec(Float)
toprgRec(Double) 

      
---------------------------------------------------------------------------
-- heterogeneous lists of inputs 
---------------------------------------------------------------------------
data head :-> tail = head :-> tail

infixr 5 :->


---------------------------------------------------------------------------
-- Function types to input list types. 
--------------------------------------------------------------------------- 
type family Ips a b
type family Ips' a 


#define ipsBase(t) \
type instance Ips' (Distrib (Array Pull (Exp t))) = Distrib (Array Pull (Exp t));

-- type instance Ips' (GlobArray (Exp t)) = GlobArray (Exp t);

ipsBase(Int)
ipsBase(Int8)
ipsBase(Int16)
ipsBase(Int32)
ipsBase(Int64)
ipsBase(Word)
ipsBase(Word8)
ipsBase(Word16)
ipsBase(Word32)
ipsBase(Word64)

ipsBase(Float)
ipsBase(Double)
ipsBase(Bool) 

-- type instance Ips a (GlobArray b) = Ips' a -- added Now 26

type instance Ips a (Final (Program b)) = Ips' a 
type instance Ips a (Program b) = Ips' a
type instance Ips a (b -> c) =  Ips' a :-> Ips b c

{- TODO:
    What about Distrib (Array p1 a1, Array p2 a2)
     (blocks of pairs of arrays) -- limit what can live inside a block  ? 


-} 
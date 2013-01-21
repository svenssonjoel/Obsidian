{-# LANGUAGE FlexibleInstances, 
             FlexibleContexts,
             MultiParamTypeClasses,
             TypeOperators,
             TypeFamilies ,
             ScopedTypeVariables, 
             CPP #-}

{- Joel Svensson 2012

  Notes:

  2013-01-08: Edited
  2012-12-10: Edited

-} 


module Obsidian.CodeGen.InOut where 

import Obsidian.Exp 
import Obsidian.Array

import Obsidian.Types
import Obsidian.Globs 
import Obsidian.Program
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

{-
instance ToProgram (Distrib (Pull (Exp t))) (GProgram a) where { \
  toProgram i f (Distrib blkf)  =      \
    ([(nom,Pointer t)],CG.runPrg (f input)) \
     where {nom = "input" ++ show i; \
            n   = len (blkf (variable "X")); \
            input = namedGlobal  nom n;}}  \
;\
instance ToProgram (Distrib (Pull (Exp t))) (Final (GProgram a)) where { \
  toProgram i f (Distrib blkf)  =      \
    ([(nom,Pointer t)],CG.runPrg (cheat (f input))) \
     where  {nom = "input" ++ show i; \
            n   = len (blkf (variable "X")); \
            input = namedGlobal  nom n;}} 


-} 

#define toprgBase(t) \
instance ToProgram (Exp t) (GProgram b) where {\
  toProgram i f a = \
    ([(nom,t)],CG.runPrg (f input)) \
    where {nom = "s" ++ show i; \
           input = variable nom;}}\
;\
instance ToProgram (GlobPull (Exp t)) (GProgram a) where { \
  toProgram i f (GlobPull ixf) = \
    ([(nom,Pointer t)],CG.runPrg (f input)) \
      where {nom = "input" ++ show i; \
             input = namedGlobal nom; }}\
;\
instance ToProgram (GlobPull (Exp t)) (Final (GProgram a)) where { \
  toProgram i f (GlobPull ixf) = \
    ([(nom,Pointer t)],CG.runPrg (cheat (f input))) \
      where {nom = "input" ++ show i; \
             input = namedGlobal nom; }}\

                                                              
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


{-
instance ToProgram b c => ToProgram (Distrib (Pull (Exp t))) (b -> c) where{\
  toProgram i f ((Distrib blkf) :-> rest) = ((nom,Pointer t):ins,prg)\
    where {\
      (ins,prg) = toProgram (i+1) (f input) rest;\
      nom = "input" ++ show i;\
      n   = len (blkf (variable "X"));\
      input = namedGlobal  nom n;}}\
-} 
 
#define toprgRec(t) \
instance ToProgram b c => ToProgram (Exp t) (b -> c) where{\
  toProgram i f (a :-> rest) = \
    ((nom,t):ins,prg) \
    where {\
      (ins,prg) = toProgram (i+1) (f input) rest;\
      nom = "s" ++ show i;\
      input = variable nom;}}\
;\
instance ToProgram b c => ToProgram (GlobPull (Exp t)) (b -> c) where{\
  toProgram i f ((GlobPull ixf) :-> rest) = ((nom,Pointer t):ins,prg)\
    where {\
      (ins,prg) = toProgram (i+1) (f input) rest;\
      nom = "input" ++ show i;\
      input = namedGlobal nom;}}\

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

-- type instance Ips' (Exp a) = Exp a

-- It seems I need to be this specific for some reason.
-- The commented line above is not enough! 
#define ipsBase(t) \
type instance Ips' (Exp t) = Exp t;\

type instance Ips' (GlobPull (Exp t)) = GlobPull (Exp t);


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
type instance Ips a (Final (GProgram b)) = Ips' a 
type instance Ips a (GProgram b) = Ips' a
type instance Ips a (b -> c) =  Ips' a :-> Ips b c

{- TODO:
    What about Distrib (Array p1 a1, Array p2 a2)
     (blocks of pairs of arrays) -- limit what can live inside a block  ? 


-} 
{-# LANGUAGE FlexibleInstances,
             OverlappingInstances,
             UndecidableInstances,
             FlexibleContexts,
             MultiParamTypeClasses,
             TypeOperators,
             TypeFamilies ,
             ScopedTypeVariables
             #-}

{- Joel Svensson 2012, 2013
   Niklas Ulvinge 2013

  Notes:

  2013-04-28: Big Changes. Allows empty lists of inputs
              that are represented by ().
              TODO: Add Niklas modifications that allow tuples in input arrays.

  2013-01-24: Changes with the new Array types in mind
  2013-01-08: Edited
  2012-12-10: Edited

-} 


module Obsidian.CodeGen.InOut where 

import Obsidian.Exp 
import Obsidian.Array

import Obsidian.Types
import Obsidian.Globs 
import Obsidian.Program
import Obsidian.Force
import Obsidian.Memory

import Obsidian.Names -- PHASE OUT! 

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


class ToProgram a where
  toProgram :: Int -> a -> InputList a -> (Inputs,CG.IM)


typeOf_ a = typeOf (Literal a)


---------------------------------------------------------------------------
-- Base cases
--------------------------------------------------------------------------- 
instance ToProgram (GProgram a) where
  toProgram i prg () = ([],CG.compileStep1 prg)

instance Scalar a => ToProgram (Push Grid l (Exp a)) where
  toProgram i p {-(Push _ p)-} a =
    let prg = do
          output <- Output $ Pointer $ typeOf_ (undefined :: a)
          p <: (\a ix -> assignOut output a ix)
    in 
     toProgram i prg a
    where
      assignOut out a ix = Assign out [ix] a

instance (Scalar a, Scalar b) => ToProgram (Push Grid l (Exp a,Exp b)) where
  toProgram i p {-(Push _ p)-} a =
    let prg = do
          out1 <- Output $ Pointer $ typeOf_ (undefined :: a)
          out2 <- Output $ Pointer $ typeOf_ (undefined :: b)
          
          p <: (\(a,b) ix -> assignOut (out1,out2) (a,b) ix)
    in 
     toProgram i prg a
    where
      assignOut (o1,o2) (a,b) ix =
        do
          Assign o1 [ix] a
          Assign o2 [ix] b
      
instance (ToProgram b, Scalar t) => ToProgram (Pull EWord32 (Exp t) -> b) where
  toProgram i f (a :- rest) = ((nom,Pointer t):(n,Word32):ins,prg)
    where
      (ins,prg) = toProgram (i+1) (f input) rest
      nom  = "input" ++ show i
      n    = "n" ++ show i
      lengthVar = variable n
      input = namedGlobal nom lengthVar
      t     = typeOf_ (undefined :: t)

instance (ToProgram b, Scalar t) => ToProgram (Pull Word32 (Exp t) -> b) where
  toProgram i f (a :- rest) = ((nom,Pointer t):ins,prg)
    where
      (ins,prg) = toProgram (i+1) (f input) rest
      nom  = "input" ++ show i
      input = namedGlobal nom  (len a) 
      t     = typeOf_ (undefined :: t)

instance (ToProgram b, Scalar t) => ToProgram ((Exp t) -> b) where
  toProgram i f (a :- rest) = ((nom,t):ins,prg)
    where
      (ins,prg) = toProgram (i+1) (f input) rest
      nom  = "input" ++ show i
      input = variable nom -- namedGlobal nom (len a) 
      t     = typeOf_ (undefined :: t)


    
---------------------------------------------------------------------------
-- heterogeneous lists of inputs 
---------------------------------------------------------------------------
data head :- tail = head :- tail

infixr 5 :-


---------------------------------------------------------------------------
-- Function types to input list types. 
---------------------------------------------------------------------------

type family InputList a

type instance InputList (a -> b)        = a :- (InputList b)
type instance InputList (Push Grid l b) = ()
type instance InputList (GProgram b)    = () 


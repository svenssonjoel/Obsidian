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

 
-} 


module Obsidian.CodeGen.Reify (ToProgram(..)) where 

import Obsidian.Exp 
import Obsidian.Array
import Obsidian.Mutable 

import Obsidian.Types
import Obsidian.Globs 
import Obsidian.Program
import Obsidian.Force
import Obsidian.Memory

import Obsidian.Names
import Obsidian.Library

import qualified Obsidian.CodeGen.Program as CG
import Obsidian.CodeGen.CompileIM

import Data.Word
import Data.Int
import qualified Data.Map as M 
      
---------------------------------------------------------------------------
-- New approach (hopefully)
---------------------------------------------------------------------------
-- "reify" Haskell functions into CG.Programs


---------------------------------------------------------------------------
--
--------------------------------------------------------------------------- 
class ToProgram a where
  toProgram :: Int -> a -> InputList a -> (Parameters,CG.IM)
  toProgram_ :: Int -> a -> (Parameters, CG.IM)


typeOf_ a = typeOf (Literal a)


---------------------------------------------------------------------------
-- Base cases
---------------------------------------------------------------------------

-- This instance is incorrect
instance ToProgram (GProgram ()) where
  -- toProgram i prg () = toProgram $ pJoin prg
  toProgram i prg () = ([],CG.compileStep1 prg) 
  -- Needs to deal with GProgram () and GProgram (Push a), GProgram (Pull a)
  -- in different ways.

  toProgram_ i prg = ([],CG.compileStep1 prg) 
  
-- This instance might fix the problem with empty kernels being generated
instance (ToProgram (Push Grid l a)) => ToProgram (GProgram (Push Grid l a)) where
  toProgram i p a = toProgram i (runPush p) a

  toProgram_ i p = toProgram_ i (runPush p) 

-- No ToProgram (GProgram (Pull a)) instance is needed. These programs
-- cannot currently be created using the API. The reason is that GProgram (Pull a)
-- implies a capability that GPUs do not have. The pulling from an array computed globally.
-- That kind of computation can not be synced and its result would be undefined. 


instance Scalar a => ToProgram (Push Grid l (Exp a)) where
  toProgram i p a =
    let outT = Pointer $ typeOf_ (undefined :: a)
        outN = "output" ++ show i
        
        prg = p <: assignOut outN
        
        (inputs,im) = toProgram (i+1) prg a
        
    in (inputs++[(outN,outT)],im) 
     
    where
      assignOut out a ix = Assign out [ix] a
  toProgram_ i p = toProgram i p () 

instance (Scalar a, Scalar b) => ToProgram (Push Grid l (Exp a,Exp b)) where
  toProgram i p a =
    let   outT1 = Pointer $ typeOf_ (undefined :: a)
          outT2 = Pointer $ typeOf_ (undefined :: b)
          outN1 = "output" ++ show i
          outN2 = "output" ++ show (i+1)
          

          prg = p <: assignOut (outN1,outN2) 
            
          (inputs,im) = toProgram (i+2) prg a
          
    in (inputs++[(outN1,outT1),(outN2,outT2)],im)
    where
      assignOut (o1,o2) (a,b) ix =
        do
          Assign o1 [ix] a
          Assign o2 [ix] b
  toProgram_ i p = toProgram i p () 

---------------------------------------------------------------------------
-- Recursive
---------------------------------------------------------------------------
          
instance (ToProgram b, Scalar t) => ToProgram (Pull EWord32 (Exp t) -> b) where
  toProgram i f (a :- rest) = ((nom,Pointer t):(n,Word32):ins,prg)
    where
      (ins,prg) = toProgram (i+1) (f input) rest
      nom  = "input" ++ show i
      n    = "n" ++ show i
      lengthVar = variable n
      input = namedGlobal nom lengthVar
      t     = typeOf_ (undefined :: t)
  toProgram_ i f = ((nom,Pointer t):(n,Word32):ins,prg)
    where
      (ins,prg) = toProgram_ (i+1) (f input)
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
  toProgram_ _ _ = error "toProgram_: static length" 


instance (ToProgram b, Scalar t) => ToProgram (Mutable Global EWord32 (Exp t) -> b) where
  toProgram i f (a :- rest) = ((nom,Pointer t):(n,Word32):ins,prg)
    where
      (ins,prg) = toProgram (i+1) (f input) rest
      nom  = "input" ++ show i
      n    = "n" ++ show i
      lengthVar = variable n
      input = namedMutable nom lengthVar
      t     = typeOf_ (undefined :: t)
  toProgram_ i f = ((nom,Pointer t):(n,Word32):ins,prg)
    where
      (ins,prg) = toProgram_ (i+1) (f input)
      nom  = "input" ++ show i
      n    = "n" ++ show i
      lengthVar = variable n
      input = namedMutable nom lengthVar
      t     = typeOf_ (undefined :: t)


instance (ToProgram b, Scalar t) => ToProgram ((Exp t) -> b) where
  toProgram i f (a :- rest) = ((nom,t):ins,prg)
    where
      (ins,prg) = toProgram (i+1) (f input) rest
      nom  = "input" ++ show i
      input = variable nom -- namedGlobal nom (len a) 
      t     = typeOf_ (undefined :: t)
  toProgram_ i f = ((nom,t):ins,prg)
    where
      (ins,prg) = toProgram_ (i+1) (f input) 
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

-- genKernelSM :: ToProgram prg => Word32 -> String -> prg -> (String, Word32)
-- genKernelSM = genKernelSpecsNL

-- genKernelSpecsNL :: ToProgram prg => Word32 -> String -> prg -> (String, Word32)
-- genKernelSpecsNL nt kn prg = (prgStr,bytesShared) 
--   where
--     prgStr = pretty 75 $ ppr $ compile PlatformCUDA (Config nt bytesShared) kn (a,rim) 
--     (a,im) = toProgram_ 0 prg
--     iml = computeLiveness im
--     (m,mm) = mmIM iml sharedMem (M.empty)
--     bytesShared = size m 
--     rim = renameIM mm iml


{-# LANGUAGE GADTs #-}

{- Joel Svensson,
   Josef Svenningsson
   2012 -} 
module Obsidian.Atomic where

import Obsidian.Exp
import Data.Word
import Data.Int


-- Anyone can extend these with new instances.
-- Not good. (I need to think about how to separate
-- low level CUDA concerns, from programmer level concerns) 
class Scalar a => AtomicInc a
instance AtomicInc Word32

class Scalar a => AtomicAdd a
instance AtomicAdd Word32
instance AtomicAdd Int32
instance AtomicAdd Word64

class Scalar a => AtomicSub a
instance AtomicSub Word32
instance AtomicSub Int32

class Scalar a => AtomicExch a
instance AtomicExch Word32
instance AtomicExch Word64
instance AtomicExch Int32



---------------------------------------------------------------------------
-- Atomic operations 
---------------------------------------------------------------------------
data Atomic a where

  -- Cuda only allows AtomicInc on the Int type
  --  (todo: figure out if CUDA int is 32 or 64 bit) 
  AtomicInc :: AtomicInc a => Atomic a

  AtomicAdd :: AtomicAdd a => Exp a -> Atomic a

  AtomicSub :: AtomicSub a => Exp a -> Atomic a 

  AtomicExch :: AtomicExch a => Exp a -> Atomic a
  
printAtomic AtomicInc = "atomicInc"

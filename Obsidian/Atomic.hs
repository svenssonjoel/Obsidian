{-# LANGUAGE GADTs #-}

{- Joel Svensson,
   Josef Svenningsson
   2012 -} 
module Obsidian.Atomic where

import Obsidian.Exp
import Data.Word
       

---------------------------------------------------------------------------
-- Atomic operations 
---------------------------------------------------------------------------
data Atomic a where

  -- Cuda only allows AtomicInc on the Int type
  --  (todo: figure out if CUDA int is 32 or 64 bit) 
  AtomicInc :: Atomic Word32 

printAtomic AtomicInc = "atomicInc"

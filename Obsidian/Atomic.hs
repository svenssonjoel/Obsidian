{-# LANGUAGE GADTs #-}

module Obsidian.GCDObsidian.Atomic where

import Obsidian.GCDObsidian.Exp

       

---------------------------------------------------------------------------
-- Atomic operations 
---------------------------------------------------------------------------
data Atomic a where
  AtomicInc :: Atomic (Data Int)

printAtomic AtomicInc = "atomicInc"

{-# LANGUAGE GADTs #-} 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-} 

{- Joel Svensson 2014

  Notes: 

-} 


module Obsidian.Dimension where

import Obsidian.Exp  

import Data.Word

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
-- a DataKind.. 
data Dimensions = ZERO | ONE | TWO | THREE   

data Dynamic s = Dynamic (Dims s EWord32) 
data Static s  = Static  (Dims s Word32)

class Shape (d :: Dimensions) where
  dimensions :: Dims d s -> Int

instance Shape ZERO where
  dimensions _ = 0

instance Shape ONE where
  dimensions _ = 0

instance Shape TWO where
  dimensions _ = 0

instance Shape THREE


class Dimension d where
  type Elt d 
  dims :: d dim -> Dims dim (Elt d) 

  extents :: d dim -> Dims dim EW32
  
  modify :: Dims dim2 (Elt d) -> d dim -> d dim2 

instance Dimension Dynamic where
  type Elt Dynamic = EW32
  dims (Dynamic s) = s

  extents (Dynamic s) = s 

  modify s1 (Dynamic s) = Dynamic s1

instance Dimension Static where
  type Elt Static = Word32

  dims (Static s) = s
  
  extents (Static (Dims1 a)) = Dims1 (fromIntegral a)
  extents (Static (Dims2 a b)) = Dims2 (fromIntegral a) (fromIntegral b)
  extents (Static (Dims3 a b c)) = Dims3 (fromIntegral a) (fromIntegral b) (fromIntegral c)
  extents (Static (Dims0)) = Dims0
  
  modify s1 (Static s) = Static s1
  
data Dims (b :: Dimensions)  a where
  Dims0 :: Dims ZERO a
  Dims1 :: a -> Dims ONE a 
  Dims2 :: a -> a -> Dims TWO a
  Dims3 :: a -> a -> a -> Dims THREE a

size :: Num a => Dims b a -> a 
size (Dims0) = 1
size (Dims1 x) = x
size (Dims2 x y) = x * y
size (Dims3 x y z) = x * y * z



---------------------------------------------------------------------------
-- Index
---------------------------------------------------------------------------

data Index b where
  Ix0 :: Index ZERO
  Ix1 :: EW32 -> Index ONE
  Ix2 :: EW32 -> EW32 -> Index TWO
  Ix3 :: EW32 -> EW32 -> EW32 -> Index THREE 

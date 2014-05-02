{-# LANGUAGE GADTs #-} 
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances #-}

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
  dimensions _ = 1

instance Shape TWO where
  dimensions _ = 2

instance Shape THREE where
   dimensions _ = 3

class Dimension d where
  type Elt d 
  dims :: d dim -> Dims dim (Elt d) 

  extents :: d dim -> Dims dim EW32
  
  modify :: Dims dim2 (Elt d) -> d dim -> d dim2

  dynamic :: d dim -> Dynamic dim

instance Dimension Dynamic where
  type Elt Dynamic = EW32
  dims (Dynamic s) = s

  extents (Dynamic s) = s 

  modify s1 (Dynamic s) = Dynamic s1

  dynamic = id

instance Dimension Static where
  type Elt Static = Word32

  dims (Static s) = s
  
  extents (Static (Dims1 a)) = Dims1 (fromIntegral a)
  extents (Static (Dims2 a b)) = Dims2 (fromIntegral a) (fromIntegral b)
  extents (Static (Dims3 a b c)) = Dims3 (fromIntegral a) (fromIntegral b) (fromIntegral c)
  extents (Static (Dims0)) = Dims0
  
  modify s1 (Static s) = Static s1

  dynamic s = Dynamic $ extents s 
  
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

instance Num a => Num (Dims ONE a) where
  (Dims1 x1) + (Dims1 x2) = Dims1 $ x1 + x2
  (Dims1 x1) - (Dims1 x2) = Dims1 $ x1 + x2
  (Dims1 x1) * (Dims1 x2) = Dims1 $ x1 + x2
  signum = undefined
  abs = undefined
  fromInteger n = Dims1 (fromInteger n) 

instance Num a => Num (Dims TWO a) where
  (Dims2 x1 y1) + (Dims2 x2 y2) = Dims2 (x1 + x2) (y1 + y2)
  (Dims2 x1 y1) - (Dims2 x2 y2) = Dims2 (x1 + x2) (y1 + y2) 
  (Dims2 x1 y1) * (Dims2 x2 y2) = Dims2 (x1 + x2) (y1 + y2) 
  signum = undefined
  abs = undefined
  fromInteger n = Dims2 n' n'
    where n' = (fromInteger n) 

instance Num a => Num (Dims THREE a) where
  (Dims3 x1 y1 z1) + (Dims3 x2 y2 z2) = Dims3 (x1 + x2) (y1 + y2) (z1 + z2) 
  (Dims3 x1 y1 z1) - (Dims3 x2 y2 z2) = Dims3 (x1 + x2) (y1 + y2) (z1 + z2) 
  (Dims3 x1 y1 z1) * (Dims3 x2 y2 z2) = Dims3 (x1 + x2) (y1 + y2) (z1 + z2) 
  signum = undefined
  abs = undefined
  fromInteger n = Dims3 n' n' n'
    where n' = (fromInteger n) 

---------------------------------------------------------------------------
-- Index
---------------------------------------------------------------------------

data Index b where
  Ix0 :: Index ZERO
  Ix1 :: EW32 -> Index ONE
  Ix2 :: EW32 -> EW32 -> Index TWO
  Ix3 :: EW32 -> EW32 -> EW32 -> Index THREE 

indexToList :: Index b -> [EW32]
indexToList Ix0 = [] 
indexToList (Ix1 e1) = [e1] 
indexToList (Ix2 e1 e2) = [e1,e2] 
indexToList (Ix3 e1 e2 e3) = [e1,e2,e3] 

index :: Scalar a => String -> Index b -> Exp a 
index name i@Ix0 = Index (name,indexToList i)
index name i@(Ix1 _) = Index (name,indexToList i)
index name i@(Ix2 _ _) = Index (name,indexToList i)
index name i@(Ix3 _ _ _) = Index (name,indexToList i) 


fromIndex :: Index d -> Dims d EW32
fromIndex Ix0 = Dims0  
fromIndex (Ix1 e1) = Dims1 e1
fromIndex (Ix2 e1 e2) = Dims2 e1 e2
fromIndex (Ix3 e1 e2 e3) = Dims3 e1 e2 e3 

toIndex :: Dims d EW32 -> Index d
toIndex Dims0 = Ix0
toIndex (Dims1 e1) = Ix1 e1
toIndex (Dims2 e1 e2) = Ix2 e1 e2
toIndex (Dims3 e1 e2 e3) = Ix3 e1 e2 e3

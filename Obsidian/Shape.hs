{-# LANGUAGE TypeOperators,
             GADTs,
             FlexibleInstances,
             TypeFamilies,
             MultiParamTypeClasses,
             FlexibleContexts,
             ScopedTypeVariables #-}

module Obsidian.Shape where

import Data.Word

import Obsidian.Exp hiding (Z) 

---------------------------------------------------------------------------
-- Shape module 
---------------------------------------------------------------------------

data Z
data tail :. head

infixl 3  :.

data Shape sh e where   
  Z :: Shape Z e
  (:.) :: Shape sh e -> e -> Shape (sh :. e) e

---------------------------------------------------------------------------
-- Type function
---------------------------------------------------------------------------
type family E x
type instance E Z = Z 
type instance E Word32 = Exp Word32
type instance E (Exp Word32) = Exp Word32
type instance E (a :. b) = E a :. E b
type instance E (Shape sh e) = Shape (E sh) (E e) 

type DIM0 e = Z
type DIM1 e = DIM0 e :. e 
type DIM2 e = DIM1 e :. e 
type DIM3 e = DIM2 e :. e 


---------------------------------------------------------------------------
-- Attempt at a shapely class !
---------------------------------------------------------------------------
class Shapely sh where
  size :: sh -> Exp Word32
  toIndex :: sh -> E sh -> Exp Word32  
  fromIndex :: sh -> Exp Word32 -> E sh

instance Shapely (Shape Z e) where
  size _ = 1
  toIndex Z _ = 0
  fromIndex Z _ = Z

------------------------------------------------------------
-- Static case
------------------------------------------------------------
instance Shapely (Shape s Word32)
         => Shapely (Shape (s :. Word32) Word32) where
  size (s :. n) = size s * fromIntegral n
  fromIndex (s :. n)  ix = fromIndex s (ix `quot` n') :. (ix `rem` n')  
    where n' = fromIntegral n
  toIndex (sh1 :. sh2) (i1 :. i2) = toIndex sh1 i1 * (fromIntegral sh2) + i2 

------------------------------------------------------------
-- Dynamic case
------------------------------------------------------------
instance Shapely (Shape s (Exp Word32))
         => Shapely (Shape (s :. (Exp Word32)) (Exp Word32)) where
  size (s :. n) = size s *  n
  fromIndex (s :. n)  ix = fromIndex s (ix `quot` n) :. (ix `rem` n)  
  toIndex (sh1 :. sh2) (i1 :. i2) = toIndex sh1 i1 * sh2 + i2
  
---------------------------------------------------------------------------
-- Dim
---------------------------------------------------------------------------
dim :: Shape sh e -> Int
dim Z = 0
dim (sh :. _) = dim sh + 1


---------------------------------------------------------------------------
-- Static 
---------------------------------------------------------------------------

class Static sh where
  sizeS :: sh -> Word32

instance Static (Shape Z Word32) where
  sizeS Z = 1
instance Static (Shape s Word32)
         => Static (Shape (s :. Word32) Word32) where 
  sizeS (s :. n) = sizeS s * n 

-- I want to be able to use Static as a requirement for certain operations.
-- Storing the array into shared memory is one such operation.

---------------------------------------------------------------------------
-- Carving in Shapes 
--------------------------------------------------------------------------- 

class Blockable sh1 sh2 sh3 where
  -- block takes a static "small" shape
  -- and a dynamic large shape
  -- and an index into the shape of shapes (the result of splitting sh2 in sh1 parts)
  -- and an index into the small shape.
  -- the result is an index into the original large shape. 
  block :: sh1 -> sh2 -> sh3 -> E sh1 -> E sh2

-- this is the "Simple" 1D carving case!  
instance Blockable (Shape (Z:.Word32) Word32) -- static shape 
                   (Shape (Z:.Exp Word32) (Exp Word32)) -- dynamic shape
                   (Shape (Z:.Exp Word32) (Exp Word32)) -- index 
 where
  block (Z:.s1)
        (Z:.s2)
        (Z:.shix3)
        (Z:.shix1) = Z:.(shix3 * fromIntegral s1 * shix1)


instance Blockable (Shape Z Word32)
                   (Shape Z (Exp Word32))
                   (Shape Z (Exp Word32))
 where 
  block s1 s2 s3 s4 = Z 


-- I'm getting a feeling that this is very complicated!
-- Maybe the best move for Obsidian is to add
-- 1D, 2D and 3D Arrays and be happy with that.



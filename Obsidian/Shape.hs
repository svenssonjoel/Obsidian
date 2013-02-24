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
-- Shape module  (SIMPLE VERSION) 
---------------------------------------------------------------------------

data Dim3 e = Dim3 e e e
data Dim2 e = Dim2 e e
data Dim1 e = Dim1 e 
data Dim0 e = Dim0 


type family IxTy a
type instance IxTy Word32 = Exp Word32
type instance IxTy (Exp Word32) = Exp Word32
type instance IxTy (Dim0 e) = Dim0 (Exp Word32)
type instance IxTy (Dim1 e) = Dim1 (Exp Word32)
type instance IxTy (Dim2 e) = Dim2 (Exp Word32) 
type instance IxTy (Dim3 e) = Dim3 (Exp Word32) 
 

class Shape d e where
  dimensions :: d e -> Int
  size       :: d e -> IxTy e  
  toIndex    :: d e -> IxTy (d e) -> Exp Word32
  fromIndex  :: d e -> IxTy e -> IxTy (d e)


-- TODO: Clean this mess upp 
instance Shape Dim0 Word32 where
  dimensions _ = 0
  size _ =  1 
  toIndex _ _ = 0
  fromIndex _ _ = Dim0

instance Shape Dim0 (Exp Word32) where
  dimensions _ = 0
  size _ =  1 
  toIndex _ _ = 0
  fromIndex _ _ = Dim0


instance Shape Dim1 Word32 where
  dimensions _ = 1
  size (Dim1 n) = fromIntegral n
  toIndex (Dim1 n) (Dim1 m) = m
  fromIndex (Dim1 n) ix = Dim1 ix

instance Shape Dim1 (Exp Word32) where
  dimensions _ = 1
  size (Dim1 n) = n
  toIndex (Dim1 n) (Dim1 m) = m
  fromIndex (Dim1 n) ix = Dim1 ix 



-- Make sure these are correct! 
instance Shape Dim2 Word32 where
  dimensions _ = 2
  size (Dim2 n m) = fromIntegral (n * m)
  toIndex (Dim2 n1 n2) (Dim2 ix1 ix2) = ix2 * fromIntegral n1 + ix1
  fromIndex (Dim2 n1 n2) ix = Dim2 (ix `mod` fromIntegral n1)
                                   (ix `div` fromIntegral n1)

instance Shape Dim2 (Exp Word32) where
  dimensions _ = 2
  size (Dim2 n m) = n * m
  toIndex (Dim2 w h) (Dim2 x y) = y * w + x
  fromIndex (Dim2 w h) ix = Dim2 (ix `mod` w)
                                 (ix `div` w)


-- TODO: Draw some pictures and implement this one: 
instance Shape Dim3 Word32 where
  dimensions _ = 3
  size (Dim3 w h d) = fromIntegral (w * h * d)
  toIndex (Dim3 w h d) (Dim3 x y z) = undefined
  fromIndex (Dim3 w h d) ix = undefined 
    
instance Shape Dim3 (Exp Word32) where
  dimensions _ = 3
  size (Dim3 w h d) = w * h * d
  toIndex (Dim3 w h d) (Dim3 x y z) = undefined
  fromIndex (Dim3 w h d) ix = undefined 
 

  


------------------------------------------------------------
-- Static case
------------------------------------------------------------
--instance Shapely (Shape s Word32)
--         => Shapely (Shape (s :. Word32) Word32) where
--  size (s :. n) = size s * fromIntegral n
--  fromIndex (s :. n)  ix = fromIndex s (ix `div` n') :. (ix `mod` n')  
--    where n' = fromIntegral n
--  toIndex (sh1 :. sh2) (i1 :. i2) = toIndex sh1 i1 * (fromIntegral sh2) + i2 

------------------------------------------------------------
-- Dynamic case
------------------------------------------------------------
--instance Shapely (Shape s (Exp Word32))
--         => Shapely (Shape (s :. (Exp Word32)) (Exp Word32)) where
--  size (s :. n) = size s *  n
--  fromIndex (s :. n)  ix = fromIndex s (ix `div` n) :. (ix `mod` n)  
--  toIndex (sh1 :. sh2) (i1 :. i2) = toIndex sh1 i1 * sh2 + i2
  
---------------------------------------------------------------------------
-- Dim
---------------------------------------------------------------------------
--dim :: Shape sh e -> Int
--dim Z = 0
--dim (sh :. _) = dim sh + 1


---------------------------------------------------------------------------
-- Static 
---------------------------------------------------------------------------

--class Static sh where
--  sizeS :: sh -> Word32

--instance Static (Shape Z Word32) where
--  sizeS Z = 1
--instance Static (Shape s Word32)
--         => Static (Shape (s :. Word32) Word32) where 
--  sizeS (s :. n) = sizeS s * n 

-- I want to be able to use Static as a requirement for certain operations.
-- Storing the array into shared memory is one such operation.

---------------------------------------------------------------------------
-- Carving in Shapes 
--------------------------------------------------------------------------- 
{- 
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

---------------------------------------------------------------------------
-- Combining shapes (or the oposite of Carving) 
--------------------------------------------------------------------------- 

-- extend :: (Shape sh1 (Word32)) -> (Shape sh2 (Word32))  

-}

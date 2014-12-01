{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-} 

module Obsidian.MultiDim where 

import Obsidian.Exp hiding (Index,Z) 
import Obsidian.Program

import Data.Word

---------------------------------------------------------------------------
-- Class extent
---------------------------------------------------------------------------
class Num a => Extent a where 
  toExtent :: a -> EW32

instance Extent Word32 where
  toExtent = fromIntegral

instance Extent EW32 where
  toExtent = id 

---------------------------------------------------------------------------
-- Multidimensional arrays for Obsidian
---------------------------------------------------------------------------

infixl 3 :.

data Z

data tail :. head

data Shape sh where
  Z :: Shape Z
  (:.) :: Extent e => Shape tail -> e -> Shape (tail :. e)

-- a concrete representation of a dynamic shape
data DynamicShape where
  DynZ :: DynamicShape
  DynSnoc :: DynamicShape -> EW32 -> DynamicShape 

-- a concrete representation of a static shape 
data StaticShape where
  StatZ :: StaticShape
  StatSnoc :: StaticShape -> Word32 -> StaticShape 

newtype Index sh = Index {unIndex :: DynamicShape} 
newtype Static sh = Static {unStatic :: StaticShape} 

class ToIndex sh where
  toIndex :: Shape sh -> Index sh

instance ToIndex Z where
  toIndex _ = Index DynZ

instance (ToIndex sh, Extent e) => ToIndex (sh :. e) where 
  toIndex (sh :. e) = Index $ DynSnoc (unIndex (toIndex sh)) (toExtent e)
  

class IsStaticShape sh where
  staticSize :: Shape sh -> Word32

  
instance IsStaticShape Z where 
  staticSize _ = 1 

instance IsStaticShape sh => IsStaticShape (sh :. Word32) where
  staticSize (sh :. e) = staticSize sh * e 


-- | Number of dimensions 
dim :: Extent e => Shape sh -> e
dim Z = 0
dim (sh :. _) = 1 + dim sh 

-- | total size, number of elements
size :: Shape sh -> EW32
size Z = 1
size (sh :. i) = size sh * (toExtent i)

flatIndex :: Shape sh -> Index sh -> EW32
flatIndex sh ix = flatIndex' sh ix
  where
    flatIndex' :: Shape sh -> Index sh -> EW32
    flatIndex' Z (Index DynZ) =  0
    flatIndex' (sh1 :. sh2) (Index (DynSnoc sh1' sh2')) = flatIndex' sh1 (Index sh1') * toExtent sh2 + toExtent sh2'
    flatIndex' _ _ = error "Woa! This cannot happen" 

unFlatIndex :: Shape sh -> EW32 -> Index sh
unFlatIndex Z _ = Index DynZ
unFlatIndex sh@(_sh :. _) ix = fromIndexOne sh ix
  where
    fromIndexOne :: Extent e => Shape (sh :. e) -> EW32 -> Index (sh :. e) 
    fromIndexOne (Z :. _) ix = Index $ DynSnoc DynZ ix
    fromIndexOne (ds@(_ :. _) :. d) ix
      = Index $ DynSnoc (unIndex $ fromIndexOne ds (ix `quot` (toExtent d)))  (ix `rem` (toExtent d))



---------------------------------------------------------------------------
-- MultiDim Pull and Push arrays 
---------------------------------------------------------------------------
data PullMD sh a = PullMD (Shape sh) (Index sh -> a) 

data PushMD t sh a = PushMD (Shape sh) ((a -> Index sh -> TProgram ()) -> Program t ()) 


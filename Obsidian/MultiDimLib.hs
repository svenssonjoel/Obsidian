{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-} 

module Obsidian.MultiDimLib where


import Obsidian.MultiDim
import Obsidian.MultiDimForce

import Obsidian.Exp hiding (Z) 

import Data.Word
---------------------------------------------------------------------------
-- Library of operations on Multidim arrays.
---------------------------------------------------------------------------





---------------------------------------------------------------------------
-- Slicing
---------------------------------------------------------------------------

data All = All
data Any sh = Any


-- Probably need separate dynamic and static slices.. :/ 
data Slice ss where
  SZ    :: Slice Z
  (::.) :: Slice s1 -> e -> Slice (s1 :. e)
  (:::) :: Slice s1 -> All -> Slice (s1 :. All)
  SAny  :: Slice (Any s1) 


-- Added a parameter compared to feldspar. 
type family FullShape ss e 
type instance FullShape Z e                    = Z
type instance FullShape (Any sh) e             = sh
type instance FullShape (s1 :. e) e            = FullShape s1 e :. e
type instance FullShape (s1 :. All) e          = FullShape s1 e :. e 

type family SliceStaticShape ss 
type instance SliceStaticShape Z = Z
type instance SliceStaticShape (Any sh) = sh 
type instance SliceStaticShape (sl :. Word32) = SliceStaticShape sl
type instance SliceStaticShape (sl :. All) = SliceStaticShape sl :. Word32


type family SliceDynamicShape ss 
type instance SliceDynamicShape Z = Z
type instance SliceDynamicShape (Any sh) = sh 
type instance SliceDynamicShape (sl :. EW32) = SliceDynamicShape sl
type instance SliceDynamicShape (sl :. All) = SliceDynamicShape sl :. EW32


sliceDynamicOfFull :: Slice ss -> Shape (FullShape ss EW32) -> Shape (SliceDynamicShape ss)
sliceDynamicOfFull SZ Z = Z
sliceDynamicOfFull SAny sh = sh
sliceDynamicOfFull (fsl ::. _) (ssl :. _) = sliceDynamicOfFull fsl ssl 


sliceStaticOfFull :: Slice ss -> Shape (FullShape ss Word32) -> Shape (SliceStaticShape ss)
sliceStaticOfFull = undefined 

-- sliceOfFull :: Slice ss -> Shape (FullShape ss) -> Shape (SliceShape ss)
-- sliceOfFull SZ Z = Z
-- sliceOfFull SAny sh = sh
-- sliceOfFull (fsl ::. _) (ssl :. _) = sliceOfFull fsl ssl
-- sliceOfFull (fsl ::: All) (ssl :. s) = sliceOfFull fsl ssl :. s
-- -- | Compute a full shape from a slice shape
-- fullOfSlice :: Slice ss -> Shape (SliceShape ss) -> Shape (FullShape ss)
-- fullOfSlice SZ Z = Z
-- fullOfSlice SAny sh = sh
-- fullOfSlice (fsl ::. n) ssl = fullOfSlice fsl ssl :. n
-- fullOfSlice (fsl ::: All) (ssl :. s) = fullOfSlice fsl ssl :. s

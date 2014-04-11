{-# LANGUAGE FlexibleContexts #-} 
module MatMul where

import Obsidian

import Prelude hiding (zipWith)

import Control.Monad
import Data.Word 
{- 

-}


matMul :: (Num a, Storable a)
        => Pull Word32 (Pull Word32 a)
        -> Pull Word32 (Pull Word32 a)
        -> Push Grid Word32 a
matMul a b = pConcat (fmap body a)
  where
    body x = matMulRow x (transpose b) 

matMulRow :: (Num a, Storable a)
           => Pull Word32 a
           -> Pull Word32 (Pull Word32 a)
           -> Push Block Word32 a
matMulRow row mat =
  tConcat (fmap (dotProd row) mat)

dotProd :: (Num a, Storable a)
           => Pull Word32 a
           -> Pull Word32 a
           -> Push Thread Word32 a
dotProd a b = singletonPush $ seqReduce (+) (zipWith (*) a b)


transpose :: Pull Word32 (Pull Word32 a) -> Pull Word32 (Pull Word32 a)
transpose arr = mkPull n1 (\i -> mkPull n2 (\j -> (arr ! j) ! i))
  where
    n2 = len arr
    n1 = len (arr ! 0)

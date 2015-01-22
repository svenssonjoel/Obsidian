{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module MatMul where

import Obsidian

import Prelude hiding (zipWith)

import Data.Word 

matMul :: (Num a, Data a)
        => Pull Word32 (Pull Word32 a)
        -> Pull Word32 (Pull Word32 a)
        -> Push Grid Word32 a
matMul a b = asGridMap body a
  where
    body x = matMulRow x (transpose b) 

matMulRow :: (Num a, Data a)
           => Pull Word32 a
           -> Pull Word32 (Pull Word32 a)
           -> Push Block Word32 a
matMulRow row mat =
  asBlockMap (dotProd row) mat 

dotProd :: (Num a, Data a)
           => Pull Word32 a
           -> Pull Word32 a
           -> Push Thread Word32 a
dotProd a b = execThread' $ seqReduce (+) (zipWith (*) a b)


transpose :: Pull Word32 (Pull Word32 a) -> Pull Word32 (Pull Word32 a)
transpose arr = mkPull n1 (\i -> mkPull n2 (\j -> (arr ! j) ! i))
  where
    n2 = len arr
    n1 = len (arr ! 0)


------------------------------------------------------------
-- Various dotproduct
------------------------------------------------------------

-- pointless generic t parameter here.
--  the push array is of length one and
--  thus no parallelism exposable.
-- you can use it to create one thread warps or blocks 
dotProd1 :: (t *<=* Block, Num a) 
         => SPull a
         -> SPull a
         -> SPush t a
dotProd1 a b = push $ fold1 (+) $ zipWith (*) a b 
            




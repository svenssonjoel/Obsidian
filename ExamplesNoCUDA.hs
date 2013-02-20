{-# LANGUAGE ScopedTypeVariables,
             FlexibleContexts,
             TypeOperators,
             TypeFamilies,
             FlexibleInstances,
             GADTs #-} 

module Examples where

--import qualified Obsidian.CodeGen.CUDA as CUDA

import Obsidian.Program
import Obsidian.Exp hiding (Z) 
import Obsidian.Types
import Obsidian.Array
import Obsidian.Shape
--import Obsidian.Library
import Obsidian.Force
--import Obsidian.CodeGen.InOut
--import Obsidian.Atomic

import Data.Word
import Data.Int
import Data.Bits

import qualified Data.Vector.Storable as V

import Control.Monad.State

import Prelude hiding (zipWith,sum,replicate)
import qualified Prelude as P 

---------------------------------------------------------------------------
-- Util 
---------------------------------------------------------------------------
-- quickPrint :: ToProgram a b => (a -> b) -> Ips a b -> IO ()
-- quickPrint prg input =
--   putStrLn $ CUDA.genKernel "kernel" prg input 

---------------------------------------------------------------------------
-- MapFusion example
---------------------------------------------------------------------------

mapFusion :: Shape sh e => Pull (sh e) EInt -> Pull (sh e) EInt
mapFusion = aMap (+1) . aMap (*2)



input1 :: Pull (Dim1 (Exp Word32))  EInt 
input1 = namedGlobal (Dim1 (variable "X")) "apa"

-- Imposes a fixed size 
splitUp :: Word32 -> Pull (Dim1 (Exp Word32)) a 
           ->  Pull (Dim1 (Exp Word32)) (Pull (Dim1 Word32) a)

           -- Clean up 
splitUp n (Pull (Dim1 m) ixf) =
  Pull (Dim1 (m `div` fromIntegral n)) $ \(Dim1 ix0) ->
  Pull (Dim1 n) $ \(Dim1 ix1) -> ixf (Dim1 (ix0 * fromIntegral n + ix1))
                                 

-- Integral is too relaxed. 
segment1D :: (Integral e, Integral e') =>
             Dim1 e'-> Pull (Dim1 e) a -> Pull (Dim1 e) (Pull (Dim1 e') a)
segment1D (Dim1 n) (Pull (Dim1 m) shf) =
  Pull (Dim1 (m `div` fromIntegral n)) $ \ (Dim1 ix0) ->
  Pull (Dim1 n) $ \(Dim1 ix1) -> shf (Dim1 (ix0 * fromIntegral n + ix1))


  
localKern :: Pull (Dim1 Word32) EInt -> BProgram (Pull (Dim1 Word32) EInt)
localKern = force . aMap (+1)


--globalKern :: (Num a, StoreOps a)
--              => Pull (Dim1 EWord32) a
--              -> Pull (Dim1 EWord32)
--                      (BProgram (Pull (Dim1 Word32) a))
--globalKern input =  aMap (force . aMap (+1)) (segment1D 256 input)

--globalKern2 :: (Num a, StoreOps a)
--              => Pull Grid (Shape (Z :. EWord32) EWord32) a
--              -> GProgram (Pull Grid (Shape (Z :. EWord32) EWord32) a)

--globalKern2 = unBlockify . globalKern



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
--import Obsidian.Force
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
           






{- 
-- splitUp + blockify should be one thing 
splitUp :: Word32 -> Pull Grid (Shape (Z:.EWord32) EWord32) a -> 
           Pull Grid (Shape (Z:.EWord32:.EWord32) EWord32)  a
splitUp n (Pull (Z:.s) shf) = Pull (Z:.(s`div`(fromIntegral n)):.(fromIntegral n))
                              $ \(Z:.ix1:.ix0) -> shf (Z:.ix1*(fromIntegral n) + ix0)

-- Generalise these ?? (scary thought) 
blockify :: Pull Grid (Shape (Z:.EWord32:.EWord32) EWord32)a ->
            Pull Grid (Shape (Z:.EWord32) EWord32) (Pull Block (Shape (Z:.Word32) Word32) a)   
blockify (Pull (Z:.s:.(Literal w)) shf) =
  Pull (Z:.s) $ \(Z:.ix1) ->
  Pull (Z:.w) $ \(Z:.ix2) -> shf (Z:.ix1:.ix2)


unBlockify :: Pull Grid (Shape (Z:.EWord32) EWord32) (BProgram (Pull Block (Shape (Z:.Word32) Word32) a))
              -> GProgram (Pull Grid (Shape (Z:.EWord32) EWord32) a)
unBlockify (Pull (Z:.s) bxf) =
  do
    ForAllBlocks s $ \bix ->
      do 
        let arr = fst $ runPrg 0 $ bxf (Z:.0) 
        let sh = shape arr
        bxf (fromIndex sh bix)
        return ()
        
    return undefined 


localKern :: Pull Block Dim1 EInt -> BProgram (Pull Block Dim1 EInt)
localKern = force . aMap (+1)

-- Apply the local kernel (a global example) 
--globalKern :: Pull Grid DynDim1 EInt -> DynDim1
--              -> BProgram (Pull Block Dim1 EInt)
--globalKern = blocks localKern (Z:.256) 

globalKern :: (Num a, StoreOps a)
              => Pull Grid (Shape (Z :. EWord32) EWord32) a
              -> Pull Grid (Shape (Z :. EWord32) EWord32)
                      (BProgram (Pull Block (Shape (Z :. Word32) Word32) a))
globalKern input =  aMap (force . aMap (+1)) (blockify (splitUp 256 input))

globalKern2 :: (Num a, StoreOps a)
              => Pull Grid (Shape (Z :. EWord32) EWord32) a
              -> GProgram (Pull Grid (Shape (Z :. EWord32) EWord32) a)

globalKern2 = unBlockify . globalKern


-- This is tricky. 
--class ToGProgram a where
--  type Global a sh2
--  toGProgram :: (Shapely sh1, Shapely sh2) => (sh1 -> BProgram a) -> sh2 -> GProgram (Global a sh2)

--instance Scalar a => ToGProgram (Pull Block sh1 (Exp a)) where
--  type Global (Pull Block sh1 (Exp a)) sh2 = Push Grid sh2 (Exp a)
--  toGProgram f sh2 = 
--    do      
--      let (pulla,_) = runPrg 0 $ f (BlockIdx X)
--      let n = sizeS $ shape  pulla

--      let nb = size sh2 

      --shared <- uniqueSM

--      ForAllBlocks nb $ \bix -> undefined

--      return undefined 
      {- 
        do
          res <- f bix -- compute res.

          -- Sync
  
          Allocate shared (n * fromIntegral (sizeOf (undefined :: Exp a)))
                          (Pointer (typeOf (undefined :: Exp a)))

          ForAll (Just n) $ \tix ->
            -- potentially unnessecary assignment...
            -- if the last thing the local computation does is force. 
            Assign shared tix (res ! tix)


          Sync
          
      return $
        GlobPush $ \wf ->
        do
          ForAllBlocks $ \bix-> 
            ForAll (Just n) $ \tix ->
              wf (index shared tix)
                 (bix * fromIntegral n + tix)

-} 

             
--input2 :: GlobPull EInt
--input2 = namedGlobal "apa" 

--input3 :: GlobPull (Exp Int32)e
--input3 = namedGlobal "apa" 


---------------------------------------------------------------------------
-- Hacking
---------------------------------------------------------------------------
--forAllT' :: GlobPull (Program Thread ()) -> Program Grid ()
--forAllT' (GlobPull gixf) = forAllT gixf

--forAllLocal :: Pull (Program Thread ()) -> Program Block ()
--forAllLocal (Pull n ixf) = ForAll (Just n) ixf 
-} 

{-# LANGUAGE ScopedTypeVariables #-} 

module Ex where

import qualified Obsidian.CodeGen.CUDA as CUDA

import qualified Foreign.CUDA.Driver as CUDA
import qualified Foreign.CUDA.Driver.Device as CUDA
import Obsidian.Run.CUDA.Exec

import Obsidian

import Data.Word
import Data.Int
import Data.Bits

import qualified Data.Vector.Storable as V

import Control.Monad.State

import Prelude hiding (zipWith,sum,replicate,take,drop)
import qualified Prelude as P 

---------------------------------------------------------------------------
-- Util 
---------------------------------------------------------------------------
quickPrint :: ToProgram prg => prg -> InputList prg -> IO ()
quickPrint prg input =
  putStrLn $ CUDA.genKernel "kernel" prg input 
 
---------------------------------------------------------------------------
-- MapFusion example
---------------------------------------------------------------------------

mapFusion :: Pull Word32 EInt32 -> BProgram (SPush Block EInt32)
mapFusion arr =
  do
    imm <- force $ (fmap (+1) . fmap (*2)) arr
    imm2 <- force $ (fmap (+3) . fmap (*4)) imm
    return $ push imm2

mapTest :: Pull Word32 EInt32 -> BProgram (SPush Block EInt32)
mapTest arr =
  do
    return $ push (fmap (+1) arr) 
  
splitUp :: (ASize l, Num l)
           => l -> Pull (Exp Word32) a -> Pull (Exp Word32) (Pull l a)
splitUp n (Pull m ixf) = Pull (m `div` fromIntegral n) $ 
                          \i -> Pull n $ \j -> ixf (i * (sizeConv n) + j)


splitUpS :: Word32 -> Pull Word32 a -> Pull Word32 (Pull Word32 a)
splitUpS n (Pull m ixf) = Pull (m `div` n) $ 
                          \i -> Pull n $ \j -> ixf (i * (fromIntegral n) + j)

--test1 :: Pull (Exp Word32) EInt -> GProgram (Push Grid (Exp Word32) EInt)
--test1 input = liftG  $ fmap mapFusion (splitUp 256 input) 

input1 :: Pull (Exp Word32) EInt32
input1 = namedGlobal "apa" (variable "X")

mf :: Pull (Exp Word32) EInt32
     -> DPush Grid EInt32
mf arr = pConcatMap mapTest (splitUp 256 arr) 

---------------------------------------------------------------------------
-- Testing integration
---------------------------------------------------------------------------

test = withCUDA $
       do
         kern <- capture mf (input1 :- ())

         useVector (V.fromList (P.replicate 256 (7::Int32))) $ \ i1 ->
           useVector (V.fromList (P.replicate 256 0)) $ \(o1 :: CUDA.DevicePtr Int32) -> 
           do
             execute kern 1 256 i1 o1
             r <- lift $ CUDA.peekListArray 256 o1
             lift $ putStrLn $ show r
             

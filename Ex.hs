{-# LANGUAGE ScopedTypeVariables #-} 

module Ex where

import qualified Obsidian.CodeGen.CUDA as CUDA

import qualified Foreign.CUDA.Driver as CUDA
import qualified Foreign.CUDA.Driver.Device as CUDA
import Obsidian.Run.CUDA.Exec

import Obsidian hiding (pullFrom)

import Obsidian.Mutable

import Data.Word
import Data.Int
import Data.Bits

import qualified Data.Vector.Storable as V

import Control.Monad.State

import Prelude hiding (zipWith,sum,replicate,take,drop,reverse)
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
mapTest arr = return $ push $ reverse  $ fmap (+1) arr
  

splitUpS :: Word32 -> Pull Word32 a -> Pull Word32 (Pull Word32 a)
splitUpS n arr = mkPull (m `div` n) $ 
                 \i -> mkPull n $ \j -> arr ! (i * (fromIntegral n) + j)
  where
    m = len arr                                               

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

         useVector (V.fromList [0..255::Int32]) $ \ i1 ->
           useVector (V.fromList (P.replicate 256 0)) $ \(o1 :: CUDA.DevicePtr Int32) -> 
           do
             execute kern 1 256 i1 o1
             r <- lift $ CUDA.peekListArray 256 o1
             lift $ putStrLn $ show r



                
---------------------------------------------------------------------------
--
---------------------------------------------------------------------------

phase :: MemoryOps a => Mutable Shared a -> Int -> (a -> a -> a) -> SPull a -> BProgram (SPull a) -- SPush Block a
phase mut i f arr =
  let parr = mkPush l $ \wf -> ForAll sl2 $ \tid ->
        do
          let ix1 = insertZero i tid
              ix2 = flipBit i ix1
              ix3 = zeroBits i ix2 - 1
          wf (arr ! ix1) ix1
          wf (f (arr ! ix3) (arr ! ix2) ) ix2
  in
   do
     forceTo mut parr
     return $ pullFrom mut
  where
    l = len arr
    l2 = l `div` 2
    sl2 = sizeConv l2


sklansky2
  :: forall a. MemoryOps a =>
     Int
     -> (a -> a -> a)
     -> Pull Word32 a
     -> Program Block (Push Block Word32 a)
sklansky2 l f arr =
  do
    (mut :: Mutable Shared a)  <- newS $ push arr -- (2^l) 
    arr2 <- seq' [phase mut i f | i <- [0..(l-1)]] arr
    return $ push arr2
seq' :: MemoryOps a
        => [SPull  a -> BProgram (SPull a)] 
        -> SPull  a
        -> BProgram (SPull a)
seq' [f] arr = f arr
seq' (f:fs) arr = 
  do
    arr2 <- f arr
    seq' fs arr2
    
compose :: MemoryOps a
           => [SPull  a -> SPush Block  a] 
           -> SPull  a
           -> BProgram (SPush Block  a)
compose [f] arr = return $ f arr
compose (f:fs) arr = 
  do
    let arr1 = f arr
    arr2 <- force arr1
    compose fs arr2

insertZero :: Int -> Exp Word32 -> Exp Word32
insertZero 0 a = a `shiftL` 1
insertZero i a = a + zeroBits i a

zeroBits :: Int -> EWord32 -> EWord32
zeroBits i a = a .&. fromIntegral (complement (oneBits i :: Word32))

flipBit :: (Num a, Bits a) => Int -> a -> a
flipBit i = (`xor` (1 `shiftL` i))

oneBits :: (Num a, Bits a) => Int -> a
oneBits i = (2^i) - 1



mapScan2 :: (Choice a, MemoryOps a) => Int -> (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapScan2 n f = pConcatMap $ sklansky2 n f


getScan2 n = namedPrint ("scanB" ++ show (2^n))  (mapScan2 n (+) . splitUp (2^n)) (input :- ())
namedPrint :: ToProgram a => String -> a -> InputList a -> IO ()
namedPrint name prg input =
  putStrLn $ CUDA.genKernel name prg input

input :: DPull EInt32
input = undefinedGlobal (variable "X")


---------------------------------------------------------------------------
-- Shared memory histograms
---------------------------------------------------------------------------

histoLocal :: Mutable Shared EWord32 -> SPull EWord32 -> BProgram (SPull EWord32)
histoLocal mut arr = undefined

  
histogram :: Mutable Global EWord32 -> DPull EWord32 -> GProgram ()
histogram mut arr =
  do
    forAll b $ \bid ->
      forAll 256 $ \tid -> 
        atomicInc (arr ! (bid * 256 + tid))  mut
        
  where 
    b = fromIntegral (mutlen mut `div` 256)


input2 :: DPull EWord32
input2 = undefinedGlobal (variable "X")


getFullHistogram = quickPrint (histogram (Mutable (4*256) (Single "apa"))) (input2 :- ())
                               
{-


-}

{-# LANGUAGE NoMonomorphismRestriction #-} 
module ExamplesNoCuda where

import qualified Obsidian.CodeGen.CUDA as CUDA

import Obsidian.Program
import Obsidian.Exp
import Obsidian.Types
import Obsidian.Array
import Obsidian.Library
import Obsidian.Force
import Obsidian.CodeGen.InOut
import Obsidian.Atomic
import Obsidian.SeqLoop
import Obsidian.Lift
import Obsidian.Memory

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
quickPrint :: ToProgram a b => (a -> b) -> Ips a b -> IO ()
quickPrint prg input =
  putStrLn $ CUDA.genKernel "kernel" prg input 

---------------------------------------------------------------------------
-- MapFusion example
---------------------------------------------------------------------------

mapFusion :: Pull Word32 EInt -> BProgram (Pull Word32 EInt)
mapFusion arr =
  do
    imm <- force $ (fmap (+1) . fmap (*2)) arr
    force $ (fmap (+3) . fmap (*4)) imm

splitUp :: (ASize l, Num l)
           => l -> Pull (Exp Word32) a -> Pull (Exp Word32) (Pull l a)
splitUp n (Pull m ixf) = Pull (m `div` fromIntegral n) $ 
                          \i -> Pull n $ \j -> ixf (i * (sizeConv n) + j)


splitUpS :: Word32 -> Pull Word32 a -> Pull Word32 (Pull Word32 a)
splitUpS n (Pull m ixf) = Pull (m `div` n) $ 
                          \i -> Pull n $ \j -> ixf (i * (fromIntegral n) + j)

test1 :: Pull (Exp Word32) EInt -> GProgram (Push Grid (Exp Word32) EInt)
test1 input = liftG  $ fmap mapFusion (splitUp 256 input) 

input1 :: Pull (Exp Word32) EInt 
input1 = namedGlobal "apa" (variable "X")

---------------------------------------------------------------------------
-- Scans 
---------------------------------------------------------------------------
sklansky :: (Choice a, MemoryOps a)
            => Int
            -> (a -> a -> a)
            -> Pull Word32 a
            -> BProgram (Pull Word32 a)
sklansky 0 op arr = return arr
sklansky n op arr =
  do 
    let arr1 = twoK (n-1) (fan op) arr
    arr2 <- force arr1
    sklansky (n-1) op arr2

fan :: (Choice a, ASize l) => (a -> a -> a) -> Pull l a -> Pull l a 
fan op arr =  a1 `conc`  fmap (op c) a2 
    where 
      (a1,a2) = halve arr
      c = a1 ! sizeConv (len a1 - 1)

sklanskyG logbs op =
  join . liftM forceG . liftG . fmap (sklansky logbs op) . splitUp (2^logbs) 

getSklansky =
  quickPrint (sklanskyG 8 (+))
             (undefinedGlobal (variable "X") :: Pull (Exp Word32) EInt32)

---------------------------------------------------------------------------
-- kStone (TEST THAT THIS IS REALLY A SCAN!) 
---------------------------------------------------------------------------
kStone :: (Choice a, MemoryOps a) 
          => Int -> (a -> a -> a) -> Pull Word32 a -> BProgram (Pull Word32 a)
kStone 0 op arr = return arr
kStone n op arr =
  do
    res <- kStone (n-1) op arr 
    let r1  = drop (2^(n-1)) res
        r1' = take (2^(n-1)) res 
        r2 = zipWith op res r1 
    force (r1' `conc` r2) 

-- Push array version 
kStoneP :: (Choice a, MemoryOps a) 
          => Int -> (a -> a -> a) -> Pull Word32 a -> BProgram (Pull Word32 a)
kStoneP 0 op arr = return arr
kStoneP n op arr =
  do
    res <- kStoneP (n-1) op arr 
    let r1  = drop (2^(n-1)) res
        r1' = take (2^(n-1)) res 
        r2 = zipWith op res r1 
    force (concP Block r1' r2) 
 


kStoneG logbs op =
  join . liftM forceG . liftG . fmap (kStone logbs op) . splitUp (2^logbs)
kStonePG logbs op =
  join . liftM forceG . liftG . fmap (kStoneP logbs op) . splitUp (2^logbs) 

getKStone =
  quickPrint (kStoneG 8 (+))
             (undefinedGlobal (variable "X") :: Pull (Exp Word32) EInt32)

getKStoneP =
  quickPrint (kStonePG 8 (+))
             (undefinedGlobal (variable "X") :: Pull (Exp Word32) EInt32)

---------------------------------------------------------------------------
-- Brent Kung
--------------------------------------------------------------------------- 
bKung :: (Choice a, MemoryOps a) 
         => (a -> a -> a) -> Pull Word32 a -> BProgram (Pull Word32 a)
bKung op arr | len arr == 1 = return arr
bKung op arr = undefined 


bKungG op =
  join . liftM forceG . liftG . fmap (bKung op) . splitUp 256

getBKung =
  quickPrint (bKungG (+))
             (undefinedGlobal (variable "X") :: Pull (Exp Word32) EInt32)


---------------------------------------------------------------------------
-- Go Towards Counting sort again.  
--------------------------------------------------------------------------- 
histogram :: Pull EWord32 EInt32 -> GProgram ()
histogram arr = do
  global <- Output $ Pointer Word32
  forAllT (len arr) $ \gix -> atomicOp global (int32ToWord32 (arr ! gix)) AtomicInc

  
atomicOp n e1 a = AtomicOp n e1 a >> return () 

getHist =
  quickPrint histogram
             (undefinedGlobal (variable "X") :: Pull (Exp Word32) EInt32)
  
reconstruct :: Pull EWord32 EWord32 -> Push Grid EWord32 EInt32
reconstruct arr = Push (len arr) f
  where
    f k = do forAllT (len arr) $ \gix ->
               let startIx = arr ! gix
               in  SeqFor (arr ! (gix+1) - startIx) $ \ix ->
                   k (word32ToInt32 gix) (ix + startIx)
getRec =
  quickPrint (forceG . reconstruct)
             (undefinedGlobal (variable "X") :: Pull (EWord32) EWord32)


---------------------------------------------------------------------------
-- Testing some sequential loop approaches
---------------------------------------------------------------------------

testFold :: Pull Word32 EWord32 -> Pull Word32 (Program Thread EWord32)
testFold arr = fmap (seqFold (+) 0) (splitUpS (32 :: Word32)  arr)

testFold2 :: Pull Word32 EWord32 -> BProgram (Pull Word32 EWord32)
testFold2 = liftB . testFold

testFold3 :: Pull EWord32 EWord32
             -> Pull EWord32 (BProgram (Pull Word32 EWord32))
testFold3 arr =  fmap (testFold2) (splitUp 256 arr)

testFold4 :: Pull EWord32 EWord32 -> Program Grid ()
testFold4 = join . liftM forceG . liftG . testFold3 

flatten :: ASize l => Pull EWord32 (Pull l a) -> Pull EWord32 a
flatten pp =
  Pull (n*m) $ \ix -> (pp ! (ix `div` m)) ! (ix `mod` m)  
  where 
    n = len pp
    m = sizeConv (len (pp ! 0))
  
inputFold :: Pull Word32 EWord32 
inputFold = namedPull "apa" 256 

inputF :: Pull EWord32 EWord32 
inputF = namedPull "apa" (variable "X") 


-- reverseglobal 
revG :: Pull EWord32 a -> Pull EWord32 a
revG arr = mkPullArray n $ \ix -> arr ! (sizeConv n - 1 - ix)
 where
   n = len arr

testRev :: Scalar a=>  Pull EWord32 (Exp a) -> GProgram () 
testRev = forceG . push Grid . revG

   
---------------------------------------------------------------------------
-- Simple 
---------------------------------------------------------------------------

s1 :: ( Num a, MemoryOps a) =>
     Pull Word32 a -> BProgram (Pull Word32 a)
s1 arr = do
  a1 <- force (fmap (+3) arr)
  a2 <- force (fmap (+2) a1) 
  force (fmap (+1) a2)  

gs1 :: (Num a, MemoryOps a) =>
     Pull EWord32 a -> Program Grid (Push Grid EWord32 a)
gs1 = liftG . (fmap s1) . splitUp 256 


getgs1 =
  quickPrint (join . liftM forceG . gs1)
             (undefinedGlobal (variable "X") :: Pull (EWord32) EWord32)

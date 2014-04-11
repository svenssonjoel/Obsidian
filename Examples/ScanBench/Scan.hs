
{-# LANGUAGE NoMonomorphismRestriction,
             ScopedTypeVariables#-}

module Scan where

import Obsidian

import Data.Word
import Data.Bits

import Control.Monad

import Prelude hiding (map,zipWith,zip,sum,replicate,take,drop,iterate,last)
import qualified Prelude as P

---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- Kernel1  (Thread acceses element tid and tid+1 
---------------------------------------------------------------------------

-- Kernel1 is just a reduction! 
kernel1 :: Storable a
           => (a -> a -> a)
           -> SPull a
           -> BProgram (SPush Block a)
kernel1 f arr
  | len arr == 1 = return (push arr)
  | otherwise    = 
    do
      let (a1,a2) = evenOdds arr
      arr' <- forcePull (zipWith f a1 a2)
      kernel1 f arr'   

mapKernel1 :: Storable a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapKernel1 f arr = pConcat (fmap body arr)
  where
    body arr = runPush (kernel1 f arr)

---------------------------------------------------------------------------
-- Sklansky 
---------------------------------------------------------------------------
    
sklansky :: (Choice a, Storable a)
            => Int
            -> (a -> a -> a)
            -> Pull Word32 a
            -> Program Block (Push Block Word32 a)
sklansky 0 op arr = return (push arr)
sklansky n op arr =
  do 
    let arr1 = binSplit (n-1) (fan op) arr
    arr2 <- forcePull arr1
    sklansky (n-1) op arr2


fan :: Choice a
       => (a -> a -> a)
       -> SPull a
       -> SPull a
fan op arr =  a1 `append` fmap (op c) a2 
    where 
      (a1,a2) = halve arr
      c = a1 ! fromIntegral (len a1 - 1)

pushM = liftM push

mapScan1 :: (Choice a, Storable a) => Int -> (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapScan1 n f arr = pConcat (fmap body arr)
  where
    body arr = runPush (sklansky n f arr)

---------------------------------------------------------------------------
-- Pushy phases for Sklansky 
---------------------------------------------------------------------------

phase :: Int
       -> (a -> a -> a)
       -> Pull Word32 a
       -> Push Block Word32 a
phase i f arr =
  mkPush l (\wf -> forAll sl2 (\tid ->
  do
    let ix1 = insertZero i tid
        ix2 = flipBit i ix1
        ix3 = zeroBits i ix2 - 1
    wf (arr ! ix1) ix1
    wf (f (arr ! ix3) (arr ! ix2) ) ix2))
  where
    l = len arr
    l2 = l `div` 2
    sl2 = fromIntegral l2


sklansky2 :: Storable a
           => Int
           -> (a -> a -> a)
           -> Pull Word32 a
           -> Program Block (Push Block Word32 a)
sklansky2 l f = compose [phase i f | i <- [0..(l-1)]]
  
compose :: Storable a
           => [Pull Word32 a -> Push Block Word32 a] 
           -> Pull Word32 a
           -> Program Block (Push Block Word32 a)
compose [f] arr = return (f arr)
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



mapScan2 :: (Choice a, Storable a) => Int -> (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapScan2 n f arr = pConcat $ fmap body arr  -- sklansky2 n f
    where
      body arr = runPush (sklansky2 n f arr)

 

-- getScan2 n = namedPrint ("scanB" ++ show (2^n))  (mapScan2 n (+) . splitUp (2^n)) (input :- ())

----------------------------------------------------------------------------
-- TWEAK LOADS
----------------------------------------------------------------------------

sklansky3 :: Storable a
           => Int
           -> (a -> a -> a)
           -> Pull Word32 a
           -> Program Block (Push Block Word32 a)
sklansky3 l f arr =
  do
    im <- force (load 2 arr)
    compose [phase i f | i <- [0..(l-1)]] im 


mapScan3 :: (Choice a, Storable a) => Int -> (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapScan3 n f arr = pConcat (fmap body arr)
  where
    body arr = runPush (sklansky3 n f arr)


--getScan3 n = namedPrint ("scanC" ++ show (2^n))  (mapScan3 n (+) . splitUp (2^n)) (input :- ())
 

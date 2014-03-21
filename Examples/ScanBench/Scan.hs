
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
kernel1 :: MemoryOps a
           => (a -> a -> a)
           -> SPull a
           -> BProgram (SPush Block a)
kernel1 f arr
  | len arr == 1 = return $ push arr
  | otherwise    = 
    do
      let (a1,a2) = evenOdds arr
      arr' <- forcePull $ zipWith f a1 a2
      kernel1 f arr'   

mapKernel1 :: MemoryOps a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapKernel1 f arr = pConcat$ (fmap body arr)
  where
    body arr = runPush (kernel1 f arr)

--getKernel1 = namedPrint "kernel1" (mapKernel1 (+) . splitUp 2048) (input :- ())

{- 
---------------------------------------------------------------------------
-- Sequential Scans  Phase1 
---------------------------------------------------------------------------

phase1 :: MemoryOps a
          => (a -> a -> a)
          -> SPull a
          -> SPush Block a
phase1 f arr = 
    pConcatMap (return . seqScan f)  (splitUp 8 arr) 


mapPhase1 :: MemoryOps a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapPhase1 f = pConcatMap $ return . phase1 f

--getPhase1 = namedPrint "phase1" (mapPhase1 (+) . splitUp 2048) (input :- ())


---------------------------------------------------------------------------
-- Sequential Scans  Phase2 
---------------------------------------------------------------------------

-- phase2 :: (Choice a, MemoryOps a) 
--            => (a -> a -> a)
--            -> SPull a
--            -> BProgram (SPush Block a)
-- phase2 f arr =
--      do
--        arr1 <- force $ phase1 f arr
--        let maxs = everyNth 8 7 arr1
--        maxs' <- sklansky 8 f maxs 
--        return $ pushFan 8 f (take 1 arr `conc` maxs') arr1 

-- pushFan :: Word32 -> (a -> a -> a) -> SPull a -> SPull a -> SPush Block a  
-- pushFan n f pm pa =
--   Push (len pa) $ \wf ->
--       forAll (fromIntegral (len pa `div` n)) $ \ tix ->
--         do
--           wf (pm ! tix) (tix*fromIntegral n) 
--           seqFor (fromIntegral (n-1)) $ \ix -> wf (f (pm ! tix) (pa ! (tix*fromIntegral n + (ix+1))))
--                              (tix*fromIntegral n + (ix+1))

-- mapPhase2 :: (Choice a, MemoryOps a) => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
-- mapPhase2 f = pConcatMap $ phase2 f

-- getPhase2 = namedPrint "phase2" (mapPhase2 (+) . splitUp 2048) (input :- ())

---------------------------------------------------------------------------
-- Parallel scan 
---------------------------------------------------------------------------

-} 

sklansky :: (Choice a, MemoryOps a)
            => Int
            -> (a -> a -> a)
            -> SPull a
            -> BProgram (SPush Block a)
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
fan op arr =  a1 `conc`  fmap (op c) a2 
    where 
      (a1,a2) = halve arr
      c = a1 ! sizeConv (len a1 - 1)

pushM = liftM push

mapScan :: (Choice a, MemoryOps a) => Int -> (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapScan n f arr = pConcat (fmap body arr)
  where
    body arr = runPush (sklansky n f arr)

{- 
--getScan n = namedPrint ("scan" ++ show (2^n))  (mapScan n (+) . splitUp (2^n)) (input :- ())

-- sklanskyG logbs op arr
--   mapG (sklansky logbs op) (splitUp (2^logbs) arr)

-- getSklansky =
--   quickPrint (sklanskyG 8 (+))
--              ((undefined :: Pull (Exp Word32) EInt32) :- ())


---------------------------------------------------------------------------
-- Pushy phases for Sklansky 
---------------------------------------------------------------------------

sklanskyPhase :: -- Num a
  Int
  -> (a -> a -> a)
  -> SPull a
  -> SPush Block a
sklanskyPhase stage f arr =
  mkPush (len arr) $ \wf ->
  do
    forAll m $ \ix ->
      do 
        wf (f (arr ! o ix) (arr ! rv ix)) (o ix)
        wf (arr ! cv ix) (cv ix)
      --
  where
    m = fromIntegral (len arr `div` 2)
   
    
    n = fromIntegral (len arr)       
    cv ix = ((ix .&. cvmask) `shiftL` 1) .|. ((complement cvmask) .&. ix)
    cvmask = fromIntegral (0xFFFFFFFF `xor` (bit-1))
    bit = ((1 :: Word32) `shiftL` (stage-1))

    o ix = (cvmask .&. ((ix `shiftL` 1) .|. (fromIntegral bit))) .|. ((complement cvmask) .&. ix)
    
    rv ix = o ix .&. (fromIntegral (complement bit)) .|. (fromIntegral (bit -1 ))
    
mapPh :: (Num a , Choice a, MemoryOps a) => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapPh f = pConcatMap $ return  . sklanskyPhase 3 f

-- getPh = namedPrint "phase2" (mapPh (+) . splitUp 256) (input :- ())

    

---------------------------------------------------------------------------
-- Starting anew
---------------------------------------------------------------------------

-- Reduction phase: (sequential reductions)  
-- Just use seqReduce. 


-- Parallel Scan Phase:
-- Sklansky


-- Sequential Scans in Parallel Phase:
-- seqScan



scan :: (Choice a, MemoryOps a)
        => (a -> a -> a)
        -> a
        -> SPull a
        -> BProgram (SPush Block a)
scan f a global =
  do
    arr <- force $ load 8 global
    im1 <- force $ pConcatMap (return . seqReduce f) (splitUp 8 arr)
    im2 <- force =<< sklansky2 10 f im1
    let im2' = singleton a `conc` im2
    im3 <- force $  pConcat $ zipWith ss im2' (splitUp 8 arr)
    return $ store 8 im3 
  where
    ss a b = return $ seqScanCin f a b 

mapScan1 f a = pConcatMap $ scan f a

--getScan1 = namedPrint "scan" (mapScan1 (+) 0 . splitUp 4096) (input :- ())



---------------------------------------------------------------------------
-- Tweak the number of threads
--------------------------------------------------------------------------- 
scan2 :: (Choice a, MemoryOps a)
        => (a -> a -> a)
        -> a
        -> SPull a
        -> BProgram (SPush Block a)
scan2 f a global =
  do
    arr <- force $ load 8 global
    im1 <- force $ pConcatMap (return . doubleRed f) (splitUp 8 arr)
    im2 <- force =<< sklansky2 10 f im1
    let im2' = singleton a `conc` im2
    im3 <- force $  pConcat $ zipWith ss (pair im2') (splitUp 8 arr)
    return $ store 8 im3
  where
    ss a b = return $ doubleScan f a b


-- scans :: (Choice a, MemoryOps a) 
--         => (a -> a -> a)
--         -> a
--         -> SPull a
--         -> BProgram (SPush Block a)
-- scans f a arr =
--   do
--     (cout,im) <- scan2 f a a1
--     (_,im2)   <- scan2 f cout a2
--     return $ concP im im2
--   where (a1,a2) = halve arr


doubleRed f arr = concP a1' a2'
  where
    (a1,a2) = halve arr
    a1' = seqReduce f a1
    a2' = seqReduce f a2

doubleScan f (c1,c2) arr = concP o1 o2 
  where 
    (a1,a2) = halve arr
    o1 = seqScanCin f c1 a1
    o2 = seqScanCin f c2 a2 
              
--mapScan2 f a = pConcatMap $ scan2 f a
--getScan2 = namedPrint "scan" (mapScan2 (+) 0 . splitUp 8192) (input :- ())

--mapScan2 f a = pConcatMap $ scans f a

--getScan2 = namedPrint "scan" (mapScan2 (+) 0 . splitUp 4096) (input :- ())


---------------------------------------------------------------------------
--
---------------------------------------------------------------------------

phase :: Int -> (a -> a -> a) -> SPull a -> SPush Block a
phase i f arr =
  mkPush l $ \wf -> ForAll sl2 $ \tid ->
  do
    let ix1 = insertZero i tid
        ix2 = flipBit i ix1
        ix3 = zeroBits i ix2 - 1
    wf (arr ! ix1) ix1
    wf (f (arr ! ix3) (arr ! ix2) ) ix2
  where
    l = len arr
    l2 = l `div` 2
    sl2 = sizeConv l2


sklansky2
  :: MemoryOps a =>
     Int
     -> (a -> a -> a)
     -> Pull Word32 a
     -> Program Block (Push Block Word32 a)
sklansky2 l f = compose [phase i f | i <- [0..(l-1)]]
  
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


-- getScan2 n = namedPrint ("scanB" ++ show (2^n))  (mapScan2 n (+) . splitUp (2^n)) (input :- ())

----------------------------------------------------------------------------
-- TWEAK LOADS
----------------------------------------------------------------------------

sklansky3 :: MemoryOps a
             => Int
             -> (a -> a -> a)
             -> SPull a
             -> BProgram (SPush Block a)
sklansky3 l f arr =
  do
    im <- force$ load 2 arr 
    compose [phase i f | i <- [0..(l-1)]] im 


mapScan3 :: (Choice a, MemoryOps a) => Int -> (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapScan3 n f = pConcatMap $ sklansky3 n f


--getScan3 n = namedPrint ("scanC" ++ show (2^n))  (mapScan3 n (+) . splitUp (2^n)) (input :- ())
-} 

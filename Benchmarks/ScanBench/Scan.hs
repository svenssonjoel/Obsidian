
{-# LANGUAGE NoMonomorphismRestriction,
             ScopedTypeVariables#-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-} 
{-# LANGUAGE GADTs #-} 

module Scan where

import Obsidian

import Data.Word
import Data.Bits

import Control.Monad

import Prelude hiding (map,zipWith,zip,sum,replicate,take,drop,iterate,last, head, tail)


---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- Kernel1  (Thread acceses element tid and tid+1 
---------------------------------------------------------------------------

-- Kernel1 is just a reduction! 
kernel1 :: Data a
        => (a -> a -> a)
        -> SPull a
        -> BProgram (SPush Block a)
kernel1 f arr
  | len arr == 1 = return $ push arr
  | otherwise    = 
    do
      let (a1,a2) = evenOdds arr
      arr' <- compute (zipWith f a1 a2)
      kernel1 f arr'   

mapKernel1 :: Data a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapKernel1 f arr = asGridMap body arr 
  where
    body arr = execBlock (kernel1 f arr)

---------------------------------------------------------------------------
-- Sklansky 
---------------------------------------------------------------------------
    
sklansky :: Data a 
         => Int
         -> (a -> a -> a)
         -> Pull Word32 a
         -> Program Block (Push Block Word32 a)
sklansky 0 op arr = return $ push arr 
sklansky n op arr =
  do let arr1 = unsafeBinSplit (n-1) (fan op) arr
     arr2 <- compute arr1
     sklansky (n-1) op arr2


fan :: Data a
    => (a -> a -> a)
    -> SPull a
    -> SPull a
fan op arr =  a1 `append` fmap (op c) a2 
    where 
      (a1,a2) = halve arr
      c = a1 ! fromIntegral (len a1 - 1)

-- pushM = liftM push

mapScan1 :: Data a => Int -> (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapScan1 n f arr = asGridMap body arr -- pConcat (fmap body arr)
  where
    body arr = execBlock (sklansky n f arr)

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
        ix3 = zeroOutBits i ix2 - 1
    wf (arr ! ix1) ix1
    wf (f (arr ! ix3) (arr ! ix2) ) ix2))
  where
    l = len arr
    l2 = l `div` 2
    sl2 = fromIntegral l2


sklansky2 :: Data a
          => Int
          -> (a -> a -> a)
          -> Pull Word32 a
          -> Program Block (Push Block Word32 a)
sklansky2 l f = compose [phase i f | i <- [0..(l-1)]]
  
compose :: Data a
           => [Pull Word32 a -> Push Block Word32 a] 
           -> Pull Word32 a
           -> Program Block (Push Block Word32 a)
compose [f] arr = return $ f arr
compose (f:fs) arr = compose fs =<< compute (f arr)
 -- do
 --   let arr1 = f arr
 --   arr2 <- force arr1
 --   compose fs arr2
-- compose (f:fs) arr = 
--   do
--     let arr1 = f arr
--     arr2 <- force arr1
--     compose fs arr2

insertZero :: Int -> Exp Word32 -> Exp Word32
insertZero 0 a = a `shiftL` 1
insertZero i a = a + zeroOutBits i a

zeroOutBits :: Int -> EWord32 -> EWord32
zeroOutBits i a = a .&. fromIntegral (complement (oneBits i :: Word32))

flipBit :: (Num a, Bits a) => Int -> a -> a
flipBit i = (`xor` (1 `shiftL` i))

oneBits :: (Num a, Bits a) => Int -> a
oneBits i = (2^i) - 1



mapScan2 :: Data a => Int -> (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapScan2 n f arr = asGridMap body arr -- pConcat $ fmap body arr  -- sklansky2 n f
    where
      body arr = execBlock (sklansky2 n f arr)

 

-- getScan2 n = namedPrint ("scanB" ++ show (2^n))  (mapScan2 n (+) . splitUp (2^n)) (input :- ())

----------------------------------------------------------------------------
-- TWEAK LOADS
----------------------------------------------------------------------------

sklansky3 :: Data a
          => Int
          -> (a -> a -> a)
          -> Pull Word32 a
          -> Program Block (Push Block Word32 a)
sklansky3 l f arr =
  do im <- compute $ load 2 arr
     compose [phase i f | i <- [0..(l-1)]] im 


mapScan3 :: Data a => Int -> (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapScan3 n f arr = asGridMap body arr -- pConcat (fmap body arr)
  where
    body arr = execBlock (sklansky3 n f arr)


--getScan3 n = namedPrint ("scanC" ++ show (2^n))  (mapScan3 n (+) . splitUp (2^n)) (input :- ())
 


----------------------------------------------------------------------------
-- KS based Scans
----------------------------------------------------------------------------
ksLocal :: Data a 
        => Int -- Stages (log n) 
        -> (a -> a -> a) -- opertor
        -> SPull a -- input data
        -> BProgram (SPush Block a) -- output computation
ksLocal 0 op arr = return $ push arr
ksLocal n op arr = do
  arr2 <- compute =<< ksLocal (n-1) op arr
  let a1   = drop m arr2
      oped = zipWith op arr2 a1 
      copy = take m arr2
      all  = copy `append` oped
  return $ push all
  where m = 2^(n-1) 
-- Little bit ugly... 
  
             
ks n op arr = asGridMap body arr -- pConcatMap body arr
  where
    body arr = execBlock (ksLocal n op arr)
    
mapScan4 :: Data a => Int -> (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapScan4 n f arr = asGridMap body arr 
  where
    body arr = execBlock (ksLocal n f arr)


-- An alternative to ksLocal
-- that uses concP for push array concatenation rather
-- than append for pull array concat. 
ksLocalP :: Data a
            => Int -- Stages (log n) 
            -> (a -> a -> a) -- opertor
            -> SPull a -- input data
            -> BProgram (SPush Block a) -- output computation
ksLocalP 0 op arr = return $ push arr
ksLocalP n op arr = do
  arr2 <- compute =<< ksLocalP (n-1) op arr
  let a1   = drop m arr2
      oped = push $ zipWith op arr2 a1 
      copy = push $ take m arr2
  return $ copy `concP` oped
  where m = 2^(n-1) 


ksP n op arr = asGridMap body arr -- pConcatMap body arr
  where
    body arr = execBlock (ksLocalP n op arr)
    
mapScan5 :: Data a => Int -> (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapScan5 n f arr = asGridMap body arr -- pConcat (fmap body arr)
  where
    body arr = execBlock (ksLocalP n f arr)





  
---------------------------------------------------------------------------
-- Variants of Local Scans that accept a carry in.
---------------------------------------------------------------------------

-- come up with a carryIn wrapper for the scans

-- sklanskyLocalCin :: (Choice a, Storable a)
--                     => Int
--                     -> (a -> a -> a)
--                     -> a
--                     -> SPull a
--                     -> BProgram (SPush Block a)
-- sklanskyLocalCin n op cin arr =
--   do
--     arr' <- force $ push $ applyToHead op cin arr
--     sklansky n op arr'
--   where
--     applyToHead op cin arr =
--       let h = fmap (op cin ) $ take 1 arr
--           b = drop 1 arr
--       in h `append` b
         
wrapCIn :: (t *<=* Block, Compute t, Data a)
           => (t2 -> (t1 -> a -> a) -> Pull Word32 a -> Program t b)
           -> t2 -> (t1 -> a -> a) -> t1 -> Pull Word32 a -> Program t b
wrapCIn scan n op cin arr =
  do
    arr' <- compute $ push $ applyToHead op cin arr
    scan n op arr'
  where
    applyToHead op cin arr =
      let h = fmap (op cin ) $ take 1 arr
          b = drop 1 arr
      in h `append` b

sklanskyCInLocal :: Data a
                    => Int
                    -> (a -> a -> a)
                    -> a
                    -> SPull a
                    -> BProgram (SPush Block a)
sklanskyCInLocal = wrapCIn sklansky


mapScanCIn1 :: Data a
               => Int
               -> (a -> a -> a)
               -> DPull a
               -> DPull (SPull a)
               -> DPush Grid a

-- Horrible code duplication (refactor) 
mapScanCIn1 n op cins arr =
  asGrid $
  zipWith (body) cins arr 
    where
      body arr1 arr2 = execBlock ((wrapCIn sklansky) n op arr1 arr2) 

mapScanCIn2 n op cins arr =
  asGrid $
  zipWith (body) cins arr 
    where
      body arr1 arr2 = execBlock ((wrapCIn sklansky2) n op arr1 arr2) 

mapScanCIn3 n op cins arr =
  asGrid $
  zipWith (body) cins arr 
    where
      body arr1 arr2 = execBlock ((wrapCIn sklansky3) n op arr1 arr2) 

mapScanCIn4 n op cins arr =
  asGrid $
  zipWith (body) cins arr 
    where
      body arr1 arr2 = execBlock ((wrapCIn ksLocal) n op arr1 arr2) 

mapScanCIn5 n op cins arr =
  asGrid $
  zipWith (body) cins arr 
    where
      body arr1 arr2 = execBlock ((wrapCIn ksLocalP) n op arr1 arr2) 



---------------------------------------------------------------------------
-- Variants of Local Scans that are "inclusive" 
---------------------------------------------------------------------------

-- wrapI :: (Storable a, Forceable t, Choice a)
--            => (t2 -> (t1 -> a -> a) -> Pull Word32 a -> Program t b)
--            -> t2 -> (t1 -> a -> a) -> t1 -> Pull Word32 a -> Program t b
wrapI scan n op a arr =
  do
    arr' <- scan n op arr
    let i = singleton a -- execBlock' (return a)
    return $ i `concP` (initP arr')
    where
      -- unsafe operation on push arrays in general
      -- but in this case we know what kind of array we are dealing with
      initP parr = mkPush (len parr - 1) (pushApp parr)
    -- Output array is !not! one element longer than input. 


-- Horrible code duplication!!! 
mapIScan1 :: Data a => Int -> (a -> a -> a) -> a -> DPull (SPull a) -> DPush Grid a
mapIScan1 n f a arr = asGridMap body arr -- pConcat (fmap body arr)
  where
    body arr = execBlock ((wrapI sklansky n f a) arr)


mapIScan2 :: Data a => Int -> (a -> a -> a) -> a -> DPull (SPull a) -> DPush Grid a
mapIScan2 n f a arr = asGridMap body arr
  where
    body arr = execBlock ((wrapI sklansky2 n f a) arr)

mapIScan3 :: Data a => Int -> (a -> a -> a) -> a -> DPull (SPull a) -> DPush Grid a
mapIScan3 n f a arr = asGridMap body arr 
  where
    body arr = execBlock ((wrapI sklansky3 n f a) arr)


mapIScan4 :: Data a => Int -> (a -> a -> a) -> a -> DPull (SPull a) -> DPush Grid a
mapIScan4 n f a arr = asGridMap body arr
  where
    body arr = execBlock ((wrapI ksLocal n f a) arr)

mapIScan5 :: Data a => Int -> (a -> a -> a) -> a -> DPull (SPull a) -> DPush Grid a
mapIScan5 n f a arr = asGridMap body arr
  where
    body arr = execBlock ((wrapI ksLocalP n f a) arr)



---------------------------------------------------------------------------
-- chained up scans within a block
---------------------------------------------------------------------------

-- sklanskyLocalPull :: Data a
--                  => Int
--                  -> (a -> a -> a)
--                  -> SPull a
--                  -> BProgram (SPull a)
-- sklanskyLocalPull 0 _ arr = return arr
-- sklanskyLocalPull n op arr =
--   do 
--     let arr1 = unsafeBinSplit (n-1) (fan op) arr
--     arr2 <- compute $ push arr1
--     sklanskyLocalPull (n-1) op arr2


-- ksLocalPull :: Data a 
--         => Int -- Stages (log n) 
--         -> (a -> a -> a) -- opertor
--         -> SPull a -- input data
--         -> BProgram (SPull a) -- output computation
-- ksLocalPull 0 op arr = return arr
-- ksLocalPull n op arr = do
--   arr2 <- compute =<< ksLocalPull (n-1) op arr
--   let a1   = shiftLeft (2^(n-1)) arr2
--       oped = zipWith op arr2 a1 
--       copy = take (2^(n-1)) arr2
--       all  = copy `append` oped
--   return all

-- ksLocalPPull :: Data a
--             => Int -- Stages (log n) 
--             -> (a -> a -> a) -- opertor
--             -> SPull a -- input data
--             -> BProgram (SPull a) -- output computation
-- ksLocalPPull 0 op arr = return arr
-- ksLocalPPull n op arr = do
--   arr2 <- compute =<< ksLocalPPull (n-1) op arr
--   let a1   = shiftLeft (2^(n-1)) arr2
--       oped = push $ zipWith op arr2 a1 
--       copy = push $ take (2^(n-1)) arr2
--   compute $ copy `concP` oped


wrapKernCin :: Data a
            => ScanKernel a 
            -> Int -> (a -> a -> a) -> a -> SPull a
            -> Program Block (a, SPush Block a)
wrapKernCin kern n op cin arr = do
  arr' <- compute $ execBlock $ liftM (fmap (op cin)) $ kern n op arr
  return (last arr', push arr') 

{- 
wrapKernCin :: Data a
            => ScanKernel a 
            -> Int -> (a -> a -> a) -> a -> SPull a
            -> Program Block (a, SPush Block a)
wrapKernCin kern n op cin arr = do
  arr'  <- compute $ applyToHead op cin arr
  arr'' <- compute $ execBlock $ kern n op arr'
  return (last arr'', push arr'')
  where
    applyToHead op cin arr =
      let h = fmap (op cin ) $ take 1 arr
          b = drop 1 arr
      in h `append` b
-} 

-- wrapKernCin :: Data a => ScanKernel a -> 
--    Int -> (a -> a -> a) -> a -> SPull a -> Program Block (a, SPush Block a)
-- wrapKernCin kern n op cin arr = do
--   arr'  <- compute (applyToHead op cin arr)
--   arr'' <- kern n op arr'
--   return (arr'' ! (fromIntegral (len arr'' - 1)), push arr'')
--   where
--     applyToHead op cin arr =
--       let h = fmap (op cin ) $ take 1 arr
--           b = drop 1 arr
--       in h `append` b


sklanskies :: Data a => CinKernel a 
sklanskies n op acc arr = sMapAccum (wrapKernCin sklansky n op) acc (splitUp (2^n) arr)

-- sklanskies :: Data a => CinKernel a 
-- sklanskies n op acc arr = sMapAccum (wrapKernCin sklanskyLocalPull n op) acc (splitUp (2^n) arr)

kss :: Data a => CinKernel a 
kss n op acc arr = sMapAccum (wrapKernCin ksLocal n op) acc (splitUp (2^n) arr)

-- kss :: Data a => CinKernel a 
-- kss n op acc arr = sMapAccum (wrapKernCin ksLocalPull n op) acc (splitUp (2^n) arr)

ksps :: Data a => CinKernel a 
ksps n op acc arr = sMapAccum (wrapKernCin ksLocalP n op) acc (splitUp (2^n) arr)

-- ksps :: Data a => CinKernel a 
-- ksps n op acc arr = sMapAccum (wrapKernCin ksLocalPPull n op) acc (splitUp (2^n) arr)

type CinKernel a = Int -> (a -> a -> a) -> a -> SPull a -> SPush Block a
type Kernel a = Int -> (a -> a -> a) -> SPull a -> SPush Block a

--type ScanKernel a = Int -> (a -> a -> a) -> SPull a -> Program Block (SPull a)

type ScanKernel a = Int -> (a -> a -> a) -> SPull a -> Program Block (SPush Block a)

seqChain :: (Num a, Data a )
            => CinKernel a
            -> Int
            -> (a -> a -> a)
            -> DPull a
            -> DPull (SPull a)
            -> DPush Grid a
seqChain kern n op accs arr = asGrid $ zipWith body accs arr
  where
    body acc a = kern n op acc a 




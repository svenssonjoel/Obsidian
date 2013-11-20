
module ExamplesNoCuda where

import qualified Obsidian.CodeGen.CUDA as CUDA

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

mapFusion :: Pull Word32 EInt -> BProgram (Pull Word32 EInt)
mapFusion arr =
  do
    imm <- force $ (fmap (+1) . fmap (*2)) arr
    force $ (fmap (+3) . fmap (*4)) imm

-- splitUp :: (ASize l, Num l)
--            => l -> Pull (Exp Word32) a -> Pull (Exp Word32) (Pull l a)
-- splitUp n (Pull m ixf) = Pull (m `div` fromIntegral n) $ 
--                           \i -> Pull n $ \j -> ixf (i * (sizeConv n) + j)

-- splitUp :: (ASize l, ASize m)
--            => l -> Pull m a -> DPull (Pull l a)
-- splitUp n (Pull m ixf) = Pull (fromIntegral m `div` fromIntegral n) $ 
--                           \i -> Pull n $ \j -> ixf (i * (sizeConv n) + j)
splitUp :: ASize l
           => Word32
           -> Pull l a
           -> Pull l (SPull a)
splitUp n arr  =
  mkPull (m `div` fromIntegral n) $ \i ->
    mkPull n $ \j -> arr ! (i * (sizeConv n) + j)
  where
    m = len arr


splitUpS :: Word32 -> Pull Word32 a -> Pull Word32 (Pull Word32 a)
splitUpS n arr = mkPull (m `div` n) $ 
                 \i -> mkPull n $ \j -> arr ! (i * (fromIntegral n) + j)
  where
    m = len arr                                      

--test1 :: Pull (Exp Word32) EInt -> GProgram (Push Grid (Exp Word32) EInt)
--test1 input = liftG  $ fmap mapFusion (splitUp 256 input) 

input1 :: Pull (Exp Word32) EInt 
input1 = namedGlobal "apa" (variable "X")

---------------------------------------------------------------------------
-- Scans 
---------------------------------------------------------------------------
{-
sklansky :: (Choice a, MemoryOps a)
            => Int
            -> (a -> a -> a)
            -> Pull Word32 a
            -> BProgram (Pull Word32 a)
sklansky 0 op arr = return arr
sklansky n op arr =
  do 
    let arr1 = binSplit (n-1) (fan op) arr
    arr2 <- force arr1
    sklansky (n-1) op arr2

-- fan :: (Choice a, ASize l) => (a -> a -> a) -> Pull l a -> Pull l a
fan :: Choice a => (a -> a -> a) -> SPull a -> SPull a
fan op arr =  a1 `conc`  fmap (op c) a2 
    where 
      (a1,a2) = halve arr
      c = a1 ! sizeConv (len a1 - 1)

sklanskyG logbs op arr =
  mapG (sklansky logbs op) (splitUp (2^logbs) arr)

getSklansky =
  quickPrint (sklanskyG 8 (+))
             ((undefined :: Pull (Exp Word32) EInt32) :- ())
-} 
---------------------------------------------------------------------------
-- kStone (TEST THAT THIS IS REALLY A SCAN!) 
---------------------------------------------------------------------------
{- 
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
 
-} 

--kStoneG logbs op =
--join . liftM forceG . liftG . fmap (kStone logbs op) . splitUp (2^logbs)
--kStonePG logbs op =
--  join . liftM forceG . liftG . fmap (kStoneP logbs op) . splitUp (2^logbs) 

--getKStone =
--  quickPrint (kStoneG 8 (+))
--             (undefinedGlobal (variable "X") :: Pull (Exp Word32) EInt32)

--getKStoneP =
--  quickPrint (kStonePG 8 (+))
--             (undefinedGlobal (variable "X") :: Pull (Exp Word32) EInt32)

---------------------------------------------------------------------------
-- Brent Kung
--------------------------------------------------------------------------- 
bKung :: (Choice a, MemoryOps a) 
         => (a -> a -> a) -> Pull Word32 a -> BProgram (Pull Word32 a)
bKung op arr | len arr == 1 = return arr
bKung op arr = undefined 


--bKungG op =
--  join . liftM forceG . liftG . fmap (bKung op) . splitUp 256

--getBKung =
--  quickPrint (bKungG (+))
--             (undefinedGlobal (variable "X") :: Pull (Exp Word32) EInt32)


---------------------------------------------------------------------------
-- Go Towards Counting sort again.  
--------------------------------------------------------------------------- 
-- histogram :: Pull EWord32 EInt32 -> GProgram ()
-- histogram arr = do
--   global <- Output $ Pointer Word32
--   forAllT (len arr) $ \gix -> atomicOp global (i32ToW32 (arr ! gix)) AtomicInc

  
-- atomicOp n e1 a = AtomicOp n e1 a >> return () 

-- getHist =
--   quickPrint histogram
--              ((undefinedGlobal (variable "X") :: Pull (Exp Word32) EInt32) :- ())
  
-- reconstruct :: Pull EWord32 EWord32 -> Push Grid EWord32 EInt32
-- reconstruct arr = mkPush (len arr) f
--   where
--     f k = do forAllT (len arr) $ \gix ->
--                let startIx = arr ! gix
--                in  seqFor (arr ! (gix+1) - startIx) $ \ix ->
--                    do 
--                      k (w32ToI32 gix) (ix + startIx)
                 
-- getRec =
--   quickPrint reconstruct
--              ((undefinedGlobal (variable "X") :: Pull (EWord32) EWord32) :- ())


---------------------------------------------------------------------------
-- Testing some sequential loop approaches
---------------------------------------------------------------------------

{- 
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
-} 
   
---------------------------------------------------------------------------
-- Simple 
---------------------------------------------------------------------------

s1 :: ( Num a, MemoryOps a) =>
     Pull Word32 a -> BProgram (Pull Word32 a)
s1 arr = do
  a1 <- force (fmap (+3) arr)
  a2 <- force (fmap (+2) a1) 
  force (fmap (+1) a2)  

--gs1 :: (Num a, MemoryOps a) =>
--     Pull EWord32 a -> Program Grid (Push Grid EWord32 a)
--gs1 = liftG . (fmap s1) . splitUp 256 


--getgs1 =
--  quickPrint (join . liftM forceG . gs1)
--             (undefinedGlobal (variable "X") :: Pull (EWord32) EWord32)


---------------------------------------------------------------------------
-- Matrix Mul
---------------------------------------------------------------------------

type SMatrix a = Pull Word32 (Pull Word32 a)

{-
transpose :: (ASize l1, ASize l2) => Pull l1 (Pull l2 a) -> Pull l2 (Pull l1 a)
transpose arr = mkPullArray m
                $ \i -> mkPullArray n
                       $ \j -> (arr ! j) ! i                                       
                                
   where
     n = len arr
     m = len (arr ! 0) 
-}
transpose :: SMatrix a -> SMatrix a
transpose arr = mkPull m
                $ \i -> mkPull n
                       $ \j -> (arr ! j) ! i                                       
                                
   where
     n = len arr
     m = len (arr ! 0) 



{-      
matMul :: (Num a1, ASize l1, ASize l, MemoryOps a1, LiftB a1)
          => Pull l1 (Pull l a1)
          -> Pull l (Pull Word32 a1) -> Program Grid (Push Grid l1 a1)    
matMul x y = liftG
             -- Pull l (BProgram (Pull l EFloat))  
             $ fmap liftB
             -- Pull l (Pull l (Program Thread EFloat))
             $ mkPullArray n
             $ \i -> mkPullArray m
                     $ \j -> cell i j 
                          
  where cell i j = seqFold (+) 0 $ zipWith (*) (x ! i) (y' ! j) 
        y' = transpose y
        n  = len x
        m  = len y'
-} 

mkMatrix n m f = mkPull n $ \i -> mkPull m $ \j -> f i j 

{-
matMul :: (Num a, MemoryOps a, LiftB a)
          => SMatrix a -> SMatrix a -> Program Grid (Push Grid Word32 a)    
matMul x y = liftG
             -- :: Pull l (BProgram (Pull l a))  
             $ fmap liftB
             -- :: Pull l (Pull l (Program Thread a))
             $ mkMatrix n m cell 
                          
  where cell i j = seqFold (+) 0 $ zipWith (*) (x ! i) (y' ! j) 
        y' = transpose y
        n  = len x
        m  = len y'
-} 

--matMul2 :: Num a 
--          => SMatrix a -> SMatrix a -> Push Grid Word32 a
{- 
matMul :: (Num c, MemoryOps c)
          => SPull (SPull c)
          -> SPull (SPull c) -> SPush Grid c
matMul x y = zipWithG body (replicate n x) (replicate m (transpose y))
  where
    n = len x
    m = len (y ! 0) 
    body a b = force (zipWithT cell a b)
    cell i j = do
      let arr = zipWith (*) i j 
      r <- seqReduce (+) arr
      return (singleton r) 
-}               
--  where cell i j = seqFold (+) 0 $ zipWith (*) (x ! i) (y' ! j) 
--        y' = transpose y
--        n  = len x
--        m  = len y'


 {- 
matMulIn  a b = matMul (toMatrix 256 256 a) (toMatrix 256 256 b)


toMatrix :: Word32 -> Word32 -> Pull Word32 a -> SMatrix a 
toMatrix n m arr = Pull n $ \i -> Pull m $ \j -> arr ! (i * (sizeConv m) + j)


getMM =
  quickPrint matMulIn
             ((undefinedGlobal (256*256) {-(variable "X")-} :: Pull Word32 EFloat) :-
              (undefinedGlobal (256*256) {-(variable "Y")-} :: Pull Word32 EFloat) :- ())

-} 
{-
getMM2 =
  quickPrint matMulIn2
             ((undefinedGlobal (256*256) {-(variable "X")-} :: Pull Word32 EFloat) :->
              (undefinedGlobal (256*256) {-(variable "Y")-} :: Pull Word32 EFloat))
-}

{- 
inc :: SPull EFloat -> SPull EFloat
inc  = fmap (+1)

getIncP = putStrLn $ genKernel "incP" incP (input :- ())

input :: DPull EFloat
input = namedGlobal "apa" (variable "X")

incP :: DPull EFloat -> DPush Grid EFloat
incP arr = mapG (return . inc) ((splitUp 512 . ixMap (vperm2 12 3 1. vperm 11 1 0)) arr)


swapBitBlocks :: Int -> Int -> Int -> Exp Word32 -> Exp Word32
swapBitBlocks l m r i = f .|. (lbs `shiftR` (m-r)) .|. (rbs `shiftL` (l-m))
  where
    f = i .&. complement (oneBitsFT r l)
    lbs = i .&. (oneBitsFT m l) 
    rbs = i .&. (oneBitsFT r m)

oneBitsFT :: Int -> Int -> Exp Word32
oneBitsFT i j = (1 `shiftL` j)  - (1 `shiftL` i)

-- r > 0   xor the bit at r-1 with all bits in the block from r to l
bitBlockXor :: Int -> Int -> Exp Word32 -> Exp Word32
bitBlockXor l r i = i `xor` (((b `shiftL` (l-r)) - b)`shiftL` 1)
  where 
    b = i .&. (1 `shiftL` r)

vperm l m r = bitBlockXor (l-1) (r+l-m-1) . swapBitBlocks l m r

vperm2 l m r = swapBitBlocks l (r+l-m) r . bitBlockXor (l-1) (r+l-m-1)
-} 


convToPush :: SPull a -> SPush Block a
convToPush arr =
  mkPush n $ \wf ->
   forAll (fromIntegral n) $ \tid -> wf (arr ! tid) tid
  where
    n = len arr                             

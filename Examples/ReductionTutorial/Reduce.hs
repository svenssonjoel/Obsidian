
{-# LANGUAGE NoMonomorphismRestriction,
             ScopedTypeVariables#-}

module Reduce where

import Obsidian

-- import qualified Obsidian.CodeGen.CUDA as CUDA

import Data.Word
import Data.Int

import Control.Monad.State

import Prelude hiding (map,zipWith,sum,replicate,take,drop,iterate)
import qualified Prelude as P

import qualified Data.Vector.Storable as V
import Obsidian.Run.CUDA.Exec

input :: DPull EInt32
input = undefinedGlobal (variable "X")                                               

---------------------------------------------------------------------------
-- Kernel1  (Thread acceses element tid and tid+1 
---------------------------------------------------------------------------

-- red1 :: Storable a
--       => (a -> a -> a)
--       -> SPull a
--       -> BProgram (SPush Block a)
-- red1 f arr
--   | len arr == 1 = return (push arr)
--   | otherwise    = 
--     do
--       let (a1,a2) = evenOdds arr
--       arr' <- forcePull (zipWith f a1 a2)
--       red1 f arr'   


red1 :: Storable a
      => (a -> a -> a)
      -> Pull Word32 a
      -> Program Block a
red1 f arr
  | len arr == 1 = return (arr ! 0)
  | otherwise    = 
    do
      let (a1,a2) = evenOdds arr
      imm <- forcePull (zipWith f a1 a2)
      red1 f imm   

mapRed1 :: Storable a => (a -> a -> a) -> Pull EWord32 (SPull a) -> Push Grid EWord32 a
mapRed1 f arr = pConcat (fmap body arr)
  where
    body arr = singletonPush (red1 f arr) 

getRed1 = putStrLn $
          genKernel 256 "red1"
            (mapRed1 (+) . splitUp 512 :: DPull EInt32 -> DPush Grid EInt32)


---------------------------------------------------------------------------
-- Kernel2 (Thread acceses element tid and tid+n )
---------------------------------------------------------------------------

red2 :: Storable a
           => (a -> a -> a)
           -> Pull Word32 a
           -> Program Block a
red2 f arr
  | len arr == 1 = return (arr ! 0) 
  | otherwise    = 
    do
      let (a1,a2) = halve arr
      arr' <- forcePull (zipWith f a1 a2)
      red2 f arr'   

mapRed2 :: Storable a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapRed2 f arr = pConcat (fmap body arr)
  where
    body arr = singletonPush (red2 f arr)

getRed2 = putStrLn $ 
          genKernel 256 "red2"
            (mapRed2 (+) . splitUp 512 :: DPull EInt32 -> DPush Grid EInt32)


---------------------------------------------------------------------------
-- Kernel3 (Thread acceses element tid and tid+n + last op optimisation
---------------------------------------------------------------------------

red3 :: Storable a
           => Word32 
           -> (a -> a -> a)
           -> Pull Word32 a
           -> Program Block a
red3 cutoff f  arr
  | len arr == cutoff =
    return (fold1 f arr ! 0) 
  | otherwise = 
    do
      let (a1,a2) = halve arr
      arr' <- forcePull (zipWith f a1 a2)
      red3 cutoff f arr'   


mapRed3 :: Storable a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapRed3 f arr = pConcat (fmap body arr)
  where
    body arr = singletonPush (red3 2 f arr)

getRed3 = putStrLn $ 
          genKernel 256 "red3"
            (mapRed3 (+) . splitUp 512 :: DPull EInt32 -> DPush Grid EInt32)



---------------------------------------------------------------------------
-- Kernel4 (Thread performs sequential computations)
-- seqReduce is a built-in primitive that results in a for loop.
-- An alternative is to use fold1 and get an unrolled loop in the
-- generated code. 
---------------------------------------------------------------------------
seqReducePush f = singletonPush . seqReduce f 

red4 :: Storable a
           => (a -> a -> a)
           -> Pull Word32 a
           -> Program Block a
red4 f arr =
  do
    arr' <- force (tConcat (fmap (seqReducePush f) (splitUp 8 arr)))
    red3 2 f arr'
    
mapRed4 :: Storable a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapRed4 f arr = pConcat (fmap body arr)
  where
    body arr = singletonPush (red4 f arr) 

getRed4 = putStrLn $
          genKernel 256 "red4"
            (mapRed4 (+) . splitUp 512 :: DPull EInt32 -> DPush Grid EInt32)


 
---------------------------------------------------------------------------
-- Kernel5 (Thread performs sequential computations, in a coalesced fashion) 
---------------------------------------------------------------------------

red5 :: Storable a
           => (a -> a -> a)
           -> Pull Word32 a
           -> Program Block a
red5 f arr =
  do
    arr' <- force (tConcat (fmap (seqReducePush f)
                           (coalesce 8 arr)))
    red3 2 f arr' 
  

mapRed5 :: Storable a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapRed5 f arr = pConcat (fmap body arr)
  where
    body arr = singletonPush (red5 f arr) 

getRed5 = putStrLn $
          genKernel 256 "red5"
            (mapRed5 (+) . splitUp 512 :: DPull EInt32 -> DPush Grid EInt32)



---------------------------------------------------------------------------
-- Kernel6 More sequential work 
---------------------------------------------------------------------------

red5' :: Storable a
           => Word32
           -> (a -> a -> a)
           -> Pull Word32 a
           -> Program Block a
red5' n f arr =
  do
    arr' <- force (tConcat (fmap (seqReducePush f) (coalesce n arr)))
    red3 2 f arr' 

red6 = red5' 16 

mapRed6 :: Storable a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapRed6 f arr = pConcat (fmap body arr) 
  where
    body arr = singletonPush (red6 f arr) 

getRed6 = putStrLn $ 
          genKernel 256 "red6"
            (mapRed6 (+) . splitUp 512 :: DPull EInt32 -> DPush Grid EInt32)



---------------------------------------------------------------------------
-- Kernel7 Even more sequential work 
---------------------------------------------------------------------------

red7 = red5' 32

-- red7 :: Storable a
--            => (a -> a -> a)
--            -> Pull Word32 a
--            -> Program Block a
-- red7 f arr =
--   do
--     arr' <- force (tConcat (fmap (seqReduce f) (coalesce 32 arr)))
--     red3 2 f arr' 
  

mapRed7 :: Storable a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapRed7 f arr = pConcat (fmap body arr) 
  where
    body arr = singletonPush (red7 f arr)

getRed7 = putStrLn $
          genKernel 256 "red7"
            (mapRed7 (+) . splitUp 512 :: DPull EInt32 -> DPush Grid EInt32)



{-
---------------------------------------------------------------------------
-- Get all Kernels 
---------------------------------------------------------------------------

{- 
getAll :: String -> IO () 
getAll fn = writeFile fn str
  where
    str = kernPrint "kernel1" (mapKernel1 (+) . splitUp 2048) (input :- ()) ++ 
          kernPrint "kernel2" (mapKernel2 (+) . splitUp 2048) (input :- ()) ++
          kernPrint "kernel3" (mapKernel3 (+) . splitUp 2048) (input :- ()) ++
          kernPrint "kernel4" (mapKernel4 (+) . splitUp 2048) (input :- ()) ++
          kernPrint "kernel5" (mapKernel5 (+) . splitUp 2048) (input :- ()) ++
          kernPrint "kernel6" (mapKernel6 (+) . splitUp 2048) (input :- ()) ++
          kernPrint "kernel7" (mapKernel7 (+) . splitUp 2048) (input :- ()) 
-}    

---------------------------------------------------------------------------
--   Larger reductions 4096 elements. All need to use Sequentiality! 
--------------------------------------------------------------------------- 

---------------------------------------------------------------------------
-- Kernel1  (Thread acceses element tid and tid+1 
---------------------------------------------------------------------------

red1l :: Storable a
           => (a -> a -> a)
           -> SPull a
           -> BProgram (SPush Block a)
red1l f arr
  | len arr == 1 = return $ push arr
  | otherwise    = 
    do
      let (a1,a2) = evenOdds arr
      arr' <- unsafeForce' $ zipWith f a1 a2
      red1l f arr'   

unsafeForce' arr | len arr > 1024 = return arr
                 | otherwise      = unsafeForce  arr 

mapRed1l f = pConcatMap $ pJoin . red1l f

--getKernel1l = namedPrint "kernel1l" (mapKernel1l (+) . splitUp 4096) (input :- ())
getRed1l = putStrLn $ fst $
          genKernelSpecsNL 1024 "kernel1"
                           (mapRed1l (+) . splitUp 4096 :: DPull EInt32 -> DPush Grid EInt32)
             
performR1l =
  withCUDA $
  do
    kern <- capture 1024 (mapRed1l (+) . splitUp 4096 :: DPull EInt32 -> DPush Grid EInt32)

    --useVector (V.fromList (P.replicate 2048 1 :: [Int32])) $ \i ->
    useVector (V.fromList [0..4095::Int32]) $ \i ->
      allocaVector 1 $ \(o :: CUDAVector Int32) ->
      do
        fill o 0
        o <== (1,kern) <> i

        r <- peekCUDAVector o
        lift $ putStrLn $ show r
---------------------------------------------------------------------------
-- Kernel2l (Thread acceses element tid and tid+n )
---------------------------------------------------------------------------

red2l :: Storable a
           => (a -> a -> a)
           -> SPull a
           -> BProgram (SPush Block a)
red2l f arr
  | len arr == 1 = return $ push arr
  | otherwise    = 
    do
      let (a1,a2) = halve arr
      arr' <- unsafeForce' $ zipWith f a1 a2
      red2l f arr'   

mapRed2l f = pConcatMap $ pJoin . red2l f

--getKernel2l = namedPrint "kernel2" (mapKernel2l (+) . splitUp 4096) (input :- ())
getRed2l = putStrLn $ fst $
          genKernelSpecsNL 1024 "k"
                           (mapRed2l (+) . splitUp 4096 :: DPull EInt32 -> DPush Grid EInt32)
             
performR2l =
  withCUDA $
  do
    kern <- capture 1024 (mapRed2l (+) . splitUp 4096 :: DPull EInt32 -> DPush Grid EInt32)

    --useVector (V.fromList (P.replicate 2048 1 :: [Int32])) $ \i ->
    useVector (V.fromList [0..4095::Int32]) $ \i ->
      allocaVector 1 $ \(o :: CUDAVector Int32) ->
      do
        fill o 0
        o <== (1,kern) <> i

        r <- peekCUDAVector o
        lift $ putStrLn $ show r

---------------------------------------------------------------------------
-- Kernel3l (Thread acceses element tid and tid+n + last op optimisation
---------------------------------------------------------------------------

red3l :: Storable a
           => (a -> a -> a)
           -> SPull a
           -> BProgram (SPush Block a)
red3l f arr
  | len arr == 2 = return $ push $ singleton $ f (arr ! 0) (arr ! 1) 
  | otherwise    = 
    do
      let (a1,a2) = halve arr
      arr' <- unsafeForce' $ zipWith f a1 a2
      red3l f arr'   

mapRed3l f = pConcatMap $ pJoin . red3l f

--getKernel3l = namedPrint "kernel3l" (mapKernel3l (+) . splitUp 4096) (input :- ())
getRed3l = putStrLn $ fst $
          genKernelSpecsNL 1024 "k"
                           (mapRed3l (+) . splitUp 4096 :: DPull EInt32 -> DPush Grid EInt32)
             
performR3l =
  withCUDA $
  do
    kern <- capture 1024 (mapRed3l (+) . splitUp 4096 :: DPull EInt32 -> DPush Grid EInt32)

    --useVector (V.fromList (P.replicate 2048 1 :: [Int32])) $ \i ->
    useVector (V.fromList [0..4095::Int32]) $ \i ->
      allocaVector 1 $ \(o :: CUDAVector Int32) ->
      do
        fill o 0
        o <== (1,kern) <> i

        r <- peekCUDAVector o
        lift $ putStrLn $ show r

---------------------------------------------------------------------------
-- Kernel4 (Thread performs sequential computations) 
---------------------------------------------------------------------------

kernel4l :: Storable a
           => (a -> a -> a)
           -> SPull a
           -> BProgram (SPush Block a)
kernel4l f arr =
  do
    arr' <- force $  pConcatMap (seqReduce f) (splitUp 16 arr)
    red3l f arr' 
  

mapKernel4l f = pConcatMap $ pJoin . kernel4l f

--getKernel4l = namedPrint "kernel4" (mapKernel4l (+) . splitUp 4096) (input :- ())


---------------------------------------------------------------------------
-- Kernel5 (Thread performs sequential computations, in a coalesced fashion) 
---------------------------------------------------------------------------

kernel5l :: Storable a
           => (a -> a -> a)
           -> SPull a
           -> BProgram (SPush Block a)
kernel5l f arr =
  do
    arr' <- force $  pConcatMap (seqReduce f)
                                (coalesce 16 arr)
    red3l f arr' 
  

mapKernel5l f = pConcatMap $ pJoin . kernel5l f

--getKernel5l = namedPrint "kernel5" (mapKernel5l (+) . splitUp 4096) (input :- ())


---------------------------------------------------------------------------
-- Kernel6l More sequential work 
---------------------------------------------------------------------------

kernel6l :: Storable a
           => (a -> a -> a)
           -> SPull a
           -> BProgram (SPush Block a)
kernel6l f arr =
  do
    arr' <- force $  pConcatMap (seqReduce f)
                                (coalesce 32 arr)
    red3l f arr' 
  

mapKernel6l f = pConcatMap $ pJoin . kernel6l f

--getKernel6l = namedPrint "kernel6" (mapKernel6l (+) . splitUp 4096 ) (input :- ())


---------------------------------------------------------------------------
-- Kernel7l More sequential work 
---------------------------------------------------------------------------

kernel7l :: Storable a
           => (a -> a -> a)
           -> SPull a
           -> BProgram (SPush Block a)
kernel7l f arr =
  do
    arr' <- force $  pConcatMap (seqReduce f)
                                (coalesce 64 arr)
    red3l f arr' 
  

mapKernel7l f = pConcatMap $ pJoin . kernel7l f

--getKernel7l = namedPrint "kernel7" (mapKernel7l (+) . splitUp 4096 ) (input :- ())


{- 
getAllL :: String -> IO () 
getAllL fn = writeFile fn str
  where
    str = kernPrint "kernel1" (mapKernel1l (+) . splitUp 4096) (input :- ())  ++ 
          kernPrint "kernel2" (mapKernel2l (+) . splitUp 4096) (input :- ())  ++
          kernPrint "kernel3" (mapKernel3l (+) . splitUp 4096) (input :- ())   ++
          kernPrint "kernel4" (mapKernel4l (+) . splitUp 4096) (input :- ()) ++
          kernPrint "kernel5" (mapKernel5l (+) . splitUp 4096) (input :- ()) ++
          kernPrint "kernel6" (mapKernel6l (+) . splitUp 4096) (input :- ()) ++
          kernPrint "kernel7" (mapKernel7l (+) . splitUp 4096) (input :- ()) 
-} 

---------------------------------------------------------------------------
-- Naive Push Reduction  
---------------------------------------------------------------------------
naiveReduceLocal :: Storable a
                    => (a -> a -> a)
                    -> SPull a
                    -> BProgram (SPush Block a)
naiveReduceLocal f arr
  | len arr == 1 = return $ push arr
  | otherwise    = 
    do
      let (a1,a2) = halve arr
      arr' <- force $ zipWith f a1 a2
      naiveReduceLocal f arr'   

naiveReductions f = pConcatMap $ pJoin . reduceLocal f

--getNaive = namedPrint "naive" (naiveReductions (+) . splitUp 2048) (input :- ())

---------------------------------------------------------------------------
-- Push Reduction  
---------------------------------------------------------------------------
-- reduceLocal f arr | len arr == 1 = return $ push Block arr

-- reduceLocal :: Storable a => (a -> a -> a) -> SPull a -> BProgram (SPush Block a)
-- reduceLocal f arr | len arr == 2 = return $ push Block arr'
--   where arr' = mkPull 1 $ \_ -> f (arr ! 0) (arr ! 1)
-- reduceLocal f arr | len arr < 32 =
--   do
--     let (a1,a2) = halve arr
--     arr' <- write $ zipWith f a1 a2
--     reduceLocal f arr'
-- reduceLocal f arr  = 
--   do
--     let (a1,a2) = halve arr
--     arr' <- force $ zipWith f a1 a2
--     reduceLocal f arr'
    
reduceLocal :: Storable a => (a -> a -> a) -> SPull a -> BProgram (SPush Block a)
reduceLocal f arr | len arr == 2 = return $ push arr'
  where arr' = mkPull 1 $ \_ -> f (arr ! 0) (arr ! 1)
reduceLocal f arr  = 
  do
    let (a1,a2) = halve arr
    arr' <- unsafeForce $ zipWith f a1 a2
    reduceLocal f arr'   


---------------------------------------------------------------------------
-- map over all blocks 
---------------------------------------------------------------------------


reductions f = pConcatMap $ pJoin . reduceLocal f

--getRed = namedPrint "warp" (reductions (+) . splitUp 2048) (input :- ())



---------------------------------------------------------------------------
-- Apply more low-level tricks to the reduction kernel!  
---------------------------------------------------------------------------
-- In progress!

-- coalescePerm :: Word32 -> SPull a -> SPull a
-- coalescePerm n arr = Pull (len arr) $ \i -> arr ! (f n i)
--   where
--     f n i = (im `mod` n') * n' + (im `div` n') + (i' * chunkSize) 
--       where n' = fromIntegral n  
--             i' = i `div` chunkSize
--             im = i `mod` chunkSize
--             chunkSize = fromIntegral $ n*n

stride :: Word32 -> SPull a -> SPull a
stride stride arr =
  mkPull (len arr) $ \i -> arr ! ( f stride i)
  where
    f s i = (i `div` s') + s' * (i `mod` s') 
      where s' = fromIntegral s

            
-- coalesce :: Word32 -> SPull a -> SPull (SPull a)
-- coalesce n (Pull m ixf) = Pull (m `div` n) $ 
--                           \i -> Pull n $ \j -> ixf (i + (sizeConv (m `div` n)) * j)

-- coalesce :: Word32 -> SPull a -> SPull (SPull a)
-- coalesce n arr =
--   mkPull s $ \i ->
--    mkPull n $ \j -> arr ! (i + fromIntegral s * j)
--   where
--     s = (len arr) `div` n 
  


-- notCoalesce :: Word32 -> SPull a -> SPull (SPull a)
-- notCoalesce n (mkPull m ixf) = mkPull (m `div` n) $ 
--                           \i -> mkPull n $ \j -> ixf (i * fromIntegral n + j )


reduce :: Storable a => (a -> a -> a) -> SPull a -> Program Block (SPush Block a)
reduce f arr =
  do
     arr' <- force $  pConcatMap (seqReduce f) (coalesce 8 arr)
     reduceLocal f arr'

reduceNC :: Storable a => (a -> a -> a) -> SPull a -> Program Block (SPush Block a)
reduceNC f arr =
  do
     arr' <- force $  pConcatMap (seqReduce f) (splitUp 8 arr)
     reduceLocal f arr' 

reductions2 :: (ASize l, Storable b)
               => (b -> b -> b)
               -> Pull l (SPull b)
               -> Push Grid l b
reductions2 f = pConcatMap $ pJoin . reduce f

reductions2NC :: (ASize l, Storable b)
               => (b -> b -> b)
               -> Pull l (SPull b)
               -> Push Grid l b
reductions2NC f = pConcatMap $ pJoin . reduceNC f


--getRed2 :: IO ()
--getRed2 = namedPrint "tricks2" (reductions2 (+) . splitUp 2048) (input :- ())

--getRed2NC :: IO ()
--getRed2NC = namedPrint "tricks2nc" (reductions2NC (+) . splitUp 2048) (input :- ())

-} 

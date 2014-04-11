{- Joel Svensson 2012, 2013, 2014 
   Mary Sheeran  2012

   Notes:
   2014-03-31: Merged Library and LibraryG 
   2013-01-24: GlobPull nolonger exists
               GlobPush is Push Grid
              
   2013-01-08: Renamed GlobArray to GlobPush 
   2013-01-02: Added toGlobArray and toGlobArrayN
   2012-12-10: Refactoring
               (adherence to new Array types and program types)  
-}

{-# LANGUAGE FlexibleInstances,
             TypeSynonymInstances,
             ScopedTypeVariables,
             TypeFamilies,
             GADTs #-}

module Obsidian.Library where 

import Obsidian.Array 
import Obsidian.Exp 
import Obsidian.Program
import Obsidian.Types

import Obsidian.Force

-- needed for threadsPerBlock analysis 
-- import qualified Obsidian.CodeGen.Program as P 

import Control.Monad

import Data.Bits 
import Data.Word

import Prelude hiding (splitAt,zipWith,replicate,reverse)

---------------------------------------------------------------------------
-- Reverse an array by indexing in it backwards
---------------------------------------------------------------------------

-- | Reverses a Pull array.
reverse :: ASize l => Pull l a -> Pull l a 
reverse arr = mkPull n (\ix -> arr ! ((sizeConv m) - ix))  
   where m = n-1
         n = len arr
         
---------------------------------------------------------------------------
-- splitAt (name clashes with Prelude.splitAt)
---------------------------------------------------------------------------

-- | Splits a Pull array at a given point. Performs no bounds checks. 
splitAt :: (Integral i, ASize l) => i -> Pull l a -> (Pull l a, Pull l a) 
splitAt n arr = (mkPull m (\ix -> arr ! ix), 
                 mkPull  (len arr - m) (\ix -> arr ! (ix + pos)))
  where pos = fromIntegral n
        m   = fromIntegral n

-- | Splits a Pull array in the middle. 
halve :: ASize l => Pull l a -> (Pull l a, Pull l a) 
halve arr = splitAt n2 arr
  where 
    n = len arr
    n2 = n `div` 2

-- | Splits a Pull array into chunks of size n. Result is a Pull of Pull arrays.
splitUp :: ASize l => Word32 -> Pull l a -> Pull l (SPull a)
splitUp n arr {-(Pull m ixf)-} =
  mkPull (len arr `div` fromIntegral n) $ \i ->
    mkPull n $ \j -> arr ! (i * (sizeConv n) + j)                                               

-- | Same as @splitUp@ but also performs a permutation of the elements. 
coalesce :: ASize l
         => Word32 -> Pull l a -> Pull l (Pull Word32 a)
coalesce n arr =
  mkPull s $ \i ->
    mkPull n $ \j -> arr ! (i + (sizeConv s) * j)
  where s = len arr `div` fromIntegral n
        
---------------------------------------------------------------------------
-- elements at even indices to fst output, odd to snd.
---------------------------------------------------------------------------
-- | Split a Pull array into its even and odd indexed elements.
evenOdds :: ASize l => Pull l a -> (Pull l a, Pull l a)
evenOdds arr = (mkPull (n-n2) (\ix -> arr ! (2*ix)) ,
                mkPull n2     (\ix -> arr ! (2*ix + 1)))
  where
    n  = len arr
    n2 = div n 2
-- | Extract the elements at even indices from a Pull array
evens :: ASize l => Pull l a -> Pull l a
evens = fst . evenOdds

-- | Extract the elements at odd indices from a Pull array 
odds :: ASize l => Pull l a -> Pull l a
odds  = snd . evenOdds

---------------------------------------------------------------------------
-- everyNth 
---------------------------------------------------------------------------
-- | Extract every nth element from a Pull array.
everyNth :: ASize l => Word32 -> Word32 -> Pull l a -> Pull l a
everyNth n m arr = mkPull n' $ \ix -> arr ! (ix * (fromIntegral n) + fromIntegral m)
  where
    n' = len arr `div` (fromIntegral n) 
  

---------------------------------------------------------------------------
-- replicate 
---------------------------------------------------------------------------
-- | Generates a Pull array of length one, containing @a@. 
singleton :: (Array a, ASize l) => e -> a l e
singleton a = replicate 1 a

-- | Generate a pull or push array usign a function from Index to element.
generate :: (Functor (a s), Array a, ASize s)
         => s -> (EWord32 -> b) -> a s b
generate n f = fmap f (iota n) 
---------------------------------------------------------------------------
-- last and first 
---------------------------------------------------------------------------
-- | Extract last element from a Pull array.
last :: ASize l => Pull l a -> a 
last arr = arr ! fromIntegral ( len arr - 1)

-- | Extract the first element from a Pull array.
first :: ASize l => Pull l a -> a
first arr = arr ! 0 

---------------------------------------------------------------------------
-- Take and Drop (what about strange sizes ? fix) 
---------------------------------------------------------------------------
-- | Take the first @n@ elements from a Pull array
take :: ASize l => l -> Pull l a -> Pull l a
take n arr = setSize n arr

-- | Drop the first @n@ elements from a Pull array
drop :: ASize l => l -> Pull l a -> Pull l a
drop n arr = setSize (len arr - n) $ ixMap (\ix -> ix + sizeConv n) arr

---------------------------------------------------------------------------
-- fold (sequential , unrolled)  
---------------------------------------------------------------------------
-- | Fold a nonempty pull array using a given operator. The result a singleton array (push or pull). 
fold1 :: Array a => (e -> e -> e) -> Pull Word32 e -> a Word32 e
fold1  f arr = replicate 1
               $ foldl1 f [arr ! (fromIntegral i) | i <- [0..(n-1)]]   
  where n = len arr


---------------------------------------------------------------------------
-- Shift arrays
---------------------------------------------------------------------------
-- shiftRight :: (ASize l, Choice a) => Word32 -> a -> Pull l a -> Pull l a
-- shiftRight dist elt arr = setSize (len arr)
--                           $ replicate (fromIntegral dist) elt `conc` arr

-- -- TODO: incorrect! 
-- shiftLeft :: (ASize l, Choice a) => Word32 -> a -> Pull l a -> Pull l a
-- shiftLeft dist elt arr = mkPull (len arr)
--                          $ \ix -> (arr `conc`  replicate (fromIntegral dist) elt)
--                                   ! (ix + fromIntegral dist) 
                         

-- opposite of evenOdds 
--shuffle :: ASize l => PT t -> Pull l a -> Pull l a -> Push t l a
--shuffle Block a1 a2 =
--  Push (len a1 + len a2) $
--    \ wf -> ForAll (sizeConv (len a1)) $
--            \ tid -> do
--              wf (a1 ! tid) (tid * 2) 
--              wf (a2 ! tid) (tid * 2 + 1) 


---------------------------------------------------------------------------
-- Concatenate the arrays
---------------------------------------------------------------------------
-- -- | Concatenate two Pull arrays (Potentially inefficient).
-- conc :: (ASize l, Choice a) => Pull l a -> Pull l a -> Pull l a 
-- conc a1 a2 = mkPull (n1+n2)
--                $ \ix -> ifThenElse (ix <* (sizeConv n1)) 
--                        (a1 ! ix) 
--                        (a2 ! (ix - (sizeConv n1)))
--   where 
--     n1 = len a1
--     n2 = len a2 

    
---------------------------------------------------------------------------
-- zipp unzipp
---------------------------------------------------------------------------
-- | Unzip implemented on Pull arrays    
unzip :: ASize l =>  Pull l (a,b) -> (Pull l a, Pull l b)       
unzip arr = (mkPull (len arr) (\ix -> fst (arr ! ix)) ,
             mkPull (len arr) (\ix -> snd (arr ! ix)) )

-- | Zip implemented on Pull arrays
zip :: ASize l => Pull l a -> Pull l b -> Pull l (a, b)             
zip arr1 arr2 = mkPull (min (len arr1) (len arr2))
                     $ \ix -> (arr1 ! ix, arr2 ! ix) 

-- | Unzip tripples. 
unzip3 :: ASize l => Pull l (a,b,c) 
           -> (Pull l a, Pull l b, Pull l c)       
unzip3 arr = (fmap (\(x,_,_) -> x) arr,
               fmap (\(_,y,_) -> y) arr,
               fmap (\(_,_,z) -> z)  arr)
             
-- | Zip three arrays
zip3 :: ASize l
        => Pull l a
        -> Pull l b
        -> Pull l c 
        -> Pull l (a,b,c)             
zip3 arr1 arr2 arr3 = 
  mkPull (minimum [len arr1, len arr2, len arr3])
  (\ix -> (arr1 ! ix, arr2 ! ix, arr3 ! ix))
    

-- | Perform elementwise operation.
zipWith :: ASize l => (a -> b -> c) -> Pull l a -> Pull l b -> Pull l c
zipWith op a1 a2 =  
  mkPull (min (len a1) (len a2))
  (\ix -> (a1 ! ix) `op` (a2 ! ix))

-- | Perform elementwise operation. 
zipWith3 :: ASize l => (a -> b -> c-> d) -> Pull l a -> Pull l b -> Pull l c -> Pull l d
zipWith3 f a1 a2 a3 =  
  mkPull (minimum [len a1,len a2,len a3]) $ 
    \ix -> f (a1 ! ix)  (a2 ! ix) (a3 ! ix)

  
---------------------------------------------------------------------------
-- pair 
---------------------------------------------------------------------------
-- | Pair up consecutive elements in a Pull array.
pair :: ASize l => Pull l a -> Pull l (a,a)
pair arr = 
  mkPull n' (\ix -> (arr ! (ix*2),arr ! (ix*2+1))) 
  where 
    n' = len arr `div` 2 


-- | Flatten a Pull array of pairs. 
unpair :: ASize l => Choice a => Pull l (a,a) -> Pull l a
unpair arr = 
    let n = len arr
    in  mkPull (2*n) (\ix -> ifThenElse ((mod ix 2) ==* 0) 
                                  (fst (arr ! (ix `shiftR` 1)))
                                  (snd (arr ! (ix `shiftR` 1)))) 


---------------------------------------------------------------------------
-- twoK (untested for proper functionality) 
---------------------------------------------------------------------------

-- | Recursively split an array in the middle. Apply an array to array computation
--   on each part. @binSplit 3@ divides the array into 8 pieces. 
binSplit ::Int
           -> (Pull Word32 a -> Pull Word32 b)
           -> Pull Word32 a
           -> Pull Word32 b 
binSplit = twoK

-- See if this should be specifically for Static size pull arrays
twoK :: Int -> (Pull Word32 a -> Pull Word32 b) -> Pull Word32 a -> Pull Word32 b 
twoK 0 f = f  -- divide 0 times and apply f
twoK n f = \arr -> 
              let arr' = mkPull lt (\i -> (f (mkPull  m (\j -> (arr ! (g i j)))) ! (h i))) 
                  m    = (len arr `shiftR` n)   --pow of two           
                  g i j = i .&. (fromIntegral (complement (m-1))) .|. j  
                  h i   = i .&. (fromIntegral (nl2-1))   -- optimize 

                  nl2   = len (f (mkPull  m (\j -> arr ! variable "X")))
                  lt    = nl2 `shiftL` n 
              in arr'

---------------------------------------------------------------------------
-- ***                          PUSHY LIBRARY                       *** ---
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- Concatenate on Push arrays 
---------------------------------------------------------------------------

-- | Concatenate two push arrays.
concP :: ASize l
         => Push t l a -> Push t l a -> Push t l a 
concP p1 p2  =
  mkPush (n1 + n2) $ \wf ->
  do
    p1 <: wf
    p2 <: \a i -> wf a (sizeConv n1 + i) 
 where 
   n1 = len p1
   n2 = len p2 


-- Implement unpair on pusharrays,
-- Impement for more tuples than pairs. 
--unpair :: ASize l => Choice a => Pull l (a,a) -> Pull l a
--unpair arr = 
--    let n = len arr
--    in  mkPull (2*n) (\ix -> ifThenElse ((mod ix 2) ==* 0) 
--                                  (fst (arr ! (ix `shiftR` 1)))
--                                  (snd (arr ! (ix `shiftR` 1)))) 




---------------------------------------------------------------------------
-- load / Store 
---------------------------------------------------------------------------
load :: Word32 -> Pull Word32 a -> Push Block Word32 a 
load n arr =
  mkPush m (\wf ->
  forAll (fromIntegral n') (\tid ->
  do
    seqFor (fromIntegral n) (\ix -> 
      wf (arr ! (tid + (ix*fromIntegral n'))) (tid + (ix*fromIntegral n')))))

  where
    m = len arr
    n' = m `div` n

store :: Word32 -> SPull a -> SPush Block a 
store = load 


-- ########################################################################
--
-- Programming the Hierarchy
--
-- ########################################################################



---------------------------------------------------------------------------
-- Parallel concatMap  
---------------------------------------------------------------------------

pConcatMap f = pConcat . fmap f
pUnCoalesceMap f = pUnCoalesce . fmap f
pConcatMapJoin f = pConcat . fmap (runPush.f)
pUnCoalesceMapJoin f = pUnCoalesce . fmap (runPush.f)
pCoalesceMap n f = pUnCoalesce . fmap f . coalesce n
pSplitMap n f = pConcat . fmap f . splitUp n

---------------------------------------------------------------------------
-- Step into the Hierarchy by distributing a
-- Thread program parameterized on a threadId over the threads
-- at a specific level in the Hierarchy. 
---------------------------------------------------------------------------

-- | A way to enter into the hierarchy
-- A bunch of Thread computations, spread across the threads of either
-- a Warp, block or grid. (or performed sequentially in a single thread) 
tConcat :: ASize l
           => Pull l (SPush Thread b)
           -> Push t l b  
tConcat arr =
  mkPush (n * fromIntegral s) $ \wf -> do
    forAll (sizeConv n) $ \tid -> 
       let wf' a ix = wf a (tid * sizeConv s + ix)
           p = arr ! tid -- f tid 
       in p <: wf'
  where
    n = len arr
    s  = len (arr ! 0) --(f (variable "tid")) -- arr

-- | Variant of @tConcat@. 
tDistribute :: ASize l
               => l
               -> (EWord32 -> SPush Thread b)
               -> Push t l b
tDistribute n f = tConcat (mkPull n f) 

      
-- | Distribute work across the parallel resources at a given level of the GPU hiearchy
pConcat :: ASize l => Pull l (SPush t a) -> Push (Step t) l a
pConcat arr =
  mkPush (n * fromIntegral rn) $ \wf ->
    distrPar (sizeConv n) $ \bix ->
      let p = arr ! bix 
          wf' a ix = wf a (bix * sizeConv rn + ix) 
          
      in p <: wf'
  where
    n  = len arr
    rn = len (arr ! 0) -- All arrays are same length

-- | Distribute work across the parallel resources at a given level of the GPU hierarchy
pDistribute :: ASize l => l -> (EWord32 -> SPush t a) -> Push (Step t) l a
pDistribute n f = pConcat (mkPull n f) 

-- | Sequential concatenation of a Pull of Push.
sConcat :: ASize l => Pull l (SPush t a) -> Push t l a
sConcat arr =
  mkPush (n * fromIntegral rn) $ \wf ->
  do
    seqFor (sizeConv n) $ \bix ->
      let p = arr ! bix -- (Push _ p) = arr ! bix
          wf' a ix = wf a (bix * sizeConv rn + ix)              
      in p <: wf'
  where 
    n  = len arr
    rn = len $ arr ! 0

-- | Variant of sConcat.
sDistribute :: ASize l => l -> (EWord32 -> SPush t a) -> Push t l a
sDistribute n f = sConcat (mkPull n f) 

-- pUnCoalesce adapted from Niklas branch.
-- | Combines work that was distributed in a Coalesced way.
-- | Applies a permutation on stores.
pUnCoalesce :: ASize l => Pull l (SPush t a) -> Push (Step t) l a
pUnCoalesce arr =
  mkPush (n * fromIntegral rn) $ \wf ->
  distrPar (sizeConv n) $ \bix ->
    let p = arr ! bix
        wf' a ix = wf a (bix * sizeConv rn + ix)
    in p <: (g wf')
  where
    n  = len arr
    rn = len $ arr ! 0
    s  = sizeConv rn 
    g wf a i = wf a (i `div` s + (i`mod`s)*(sizeConv n))

---------------------------------------------------------------------------
-- RunPush 
---------------------------------------------------------------------------

-- | Fuses the program that computes a Push array into the Push array. 
runPush :: Program t (Push t s a) -> Push t s a
runPush prg =
  mkPush n $ \wf -> do
    parr <- prg
    parr <: wf
    -- It is a bit scary that I need to "evaluate" programs here. 
  where n = len $ fst $ runPrg 0 prg

-- | Lifts @runPush@ to one input functions.
runPush1 :: (a -> Program t (Push t s b)) -> a -> Push t s b
runPush1 f a = runPush (f a)

-- | Lifts @runPush@ to two input functions.
runPush2 :: (a -> b -> Program t (Push t s c)) -> a -> b -> Push t s c
runPush2 f a b = runPush (f a b)  

-- | Converts a program computing a pull Array to a Push array
runPull :: (Pushable t, ASize s) => Program t (Pull s a) -> Push t s a
runPull = runPush . liftM push 

-- | Lifts @runPull@ to one input functions.
runPull1 :: (Pushable t, ASize s) => (a -> Program t (Pull s b)) -> a -> Push t s b
runPull1 f a = runPull (f a)

-- | Lifts @runPull@ to two input functions.
runPull2 :: (Pushable t, ASize s) => (a -> b -> Program t (Pull s c)) -> a -> b -> Push t s c
runPull2 f a b = runPull (f a b)

---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------
pushPrg :: Program t a -> SPush t a
pushPrg = singletonPush


---------------------------------------------------------------------------
-- Singleton push arrays 
---------------------------------------------------------------------------

-- Danger! use only with Scalar a's 
-- -- | Create a singleton Push array.
--singletonPush :: a -> SPush t a
--singletonPush = singletonPushP . return 

-- | Monadic version of @singleton@.
singletonPush :: Program t a -> SPush t a
singletonPush prg =
  mkPush 1 $ \wf -> do
    a <- prg
    forAll 1 $ \ix -> 
      wf a 0


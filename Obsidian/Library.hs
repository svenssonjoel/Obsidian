{- Joel Svensson 2012..2017
   Mary Sheeran  2012

   Notes:
   2017-04-22: Cleanup
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Obsidian.Library
       (logBaseI
       , reverse
       , splitAt
       , splitUp
       , coalesce
       , halve
       , evenOdds
       , evens
       , odds
       , everyNth
       , singleton
       , generate
       , last
       , first
       , take
       , drop
       , head
       , tail
       , fold1
       , shiftLeft
       , unzip
       , zip
       , unzip3
       , zip3
       , zipWith
       , zipWith3
       , pair
       , unpair
       , unsafeBinSplit
       , binSplit
       , concP
       , unpairP
       , load   -- RENAME THIS
       , store  -- RENAME THIS
       -- Hierarchy programming
       , asThread
       , asThreadMap
       , asGrid
       , asGridMap
       , AsWarp(..)
       , AsBlock(..)
       , liftPar -- generic hierarchy programming
       , liftSeq -- generic hierarchy programming
       , liftIn  -- generic hierarchy programming
       -- Repeat a program
       , rep

       -- Executing programs
       , ExecProgram(..)
       , execThread
       , execThread'
       , execWarp
       , execWarp'
       , execBlock
       , execBlock'

       -- Leftovers from past days
       , singletonPush
       , runPush
       )where

import Obsidian.Array
import Obsidian.Exp
import Obsidian.Program
import Obsidian.Data
import Obsidian.Mutable

import Control.Monad

import Data.Bits
import Data.Word

import Prelude hiding (splitAt,zipWith,replicate,reverse,unzip,zip,zip3,unzip3,zipWith3, last, take, drop, head, tail)


---------------------------------------------------------------------------
-- Helper
---------------------------------------------------------------------------
logBaseI :: Integral a => a -> a -> a
logBaseI b x
  = if x < b
    then 0
    else
      let
        l = 2 * logBaseI (b*b) x
        doDiv x l = if x < b then l else doDiv (x`div`b) (l+1)
      in
       doDiv (x`div`(b^l)) l


---------------------------------------------------------------------------
-- Reverse an array by indexing in it backwards
---------------------------------------------------------------------------

-- | Reverses a Pull array.
reverse :: (Array array, ArrayLength array, ASize l) => array l a -> array l a
reverse arr = ixMap (\ix -> (sizeConv m) - ix) arr
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
splitUp :: (ASize l, ASize s, Integral s) => s -> Pull l a -> Pull l (Pull s a)
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

-- | Generate a pull or push array using a function from Index to element.
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
-- Head and Tail  on pull arrays
---------------------------------------------------------------------------

head :: ASize l =>  Pull l a -> a
head arr = arr ! 0

tail :: ASize l => Pull l a -> Pull l a
tail = drop 1


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
shiftLeft :: ASize l =>  Word32 -> Pull l a -> Pull l a
shiftLeft dist arr = setSize (len arr - (fromIntegral dist))
                      $ ixMap (\ix -> ix + (fromIntegral dist)) arr

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

-- | Triple up consecutive elements in a Pull array.
triple :: ASize l => Pull l a -> Pull l (a,a,a)
triple arr =
  mkPull (len arr `div` 3) $ \ix ->
    (arr ! (ix*3), arr ! (ix*3+1), arr ! (ix*3+2))

-- | Flatten a Pull array of triples.
untriple :: ASize l => Choice a => Pull l (a,a,a) -> Pull l a
untriple arr =
  mkPull (3*len arr) $ \ix ->
    let (k,j) = divMod ix 3
        (a0,a1,a2) = arr ! k
    in  ifThenElse (j ==* 0) a0 $
        ifThenElse (j ==* 1) a1 a2


-- | Quadruple up consecutive elements in a Pull array.
quadruple :: ASize l => Pull l a -> Pull l (a,a,a,a)
quadruple arr =
  mkPull (len arr `div` 4) $ \ix ->
    (arr ! (ix*4), arr ! (ix*4+1), arr ! (ix*4+2), arr ! (ix*4+3))

-- | Flatten a Pull array of triples.
unquadruple :: ASize l => Choice a => Pull l (a,a,a,a) -> Pull l a
unquadruple = unpair . unpair . fmap (\(a0,a1,a2,a3) -> ((a0,a1), (a2,a3)))



---------------------------------------------------------------------------
-- twoK (untested for proper functionality)
---------------------------------------------------------------------------

-- | Recursively split an array in the middle. Apply an array to array computation
--   on each part. @binSplit 3@ divides the array into 8 pieces.
--   UNSAFE
unsafeBinSplit :: Int
           -> (Pull Word32 a -> Pull Word32 b)
           -> Pull Word32 a
           -> Pull Word32 b
unsafeBinSplit = twoK

binSplit :: Data a
            => Int
            -> (Pull Word32 a -> Pull Word32 b)
            -> Mutable Shared Word32 a
            -> Pull Word32 b
binSplit n f = unsafeBinSplit n f . mutableToPull

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

-- | Flatten a Pull array of pairs. Result is a push array
unpairP :: ASize l => Choice a => Push t l (a,a) -> Push t l a
unpairP arr =
  mkPush (2 * len arr) $ \ wf ->
  do
    -- even iterations
    arr <: \ (a,_) i -> wf a ((sizeConv i) `shiftL` 1)
    -- Odd iterations
    arr <: \ (_,b) i -> wf b ((sizeConv i) `shiftL` 1 + 1)


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

-- "Compute as" family of functions

liftPar :: ASize l => Pull l (SPush t a) -> Push (Step t) l a
liftPar = pConcat

liftSeq :: ASize l => Pull l (SPush t a) -> Push t l a
liftSeq = sConcat

liftIn :: (t *<=* Block, ASize l)
      => Pull l (SPush Thread b)
      -> Push t l b
liftIn = tConcat

---------------------------------------------------------------------------
-- AsBlock
---------------------------------------------------------------------------
-- Rename this to asBlock, asGrid, asThread
class (t *<=* Block) => AsBlock t where
  asBlock :: SPull (SPush t a) ->
             SPush Block a
  asBlockMap :: (a -> SPush t b)
              -> SPull a
              -> SPush Block b

instance AsBlock Thread where
  asBlock = tConcat
  asBlockMap f = tConcat . fmap f

instance AsBlock Warp where
  asBlock = pConcat
  asBlockMap f = pConcat . fmap f

instance AsBlock Block where
  asBlock = sConcat
  asBlockMap f = sConcat . fmap f

---------------------------------------------------------------------------
-- AsWarp
---------------------------------------------------------------------------
class (t *<=* Warp) => AsWarp t where
  asWarp :: SPull (SPush t a) ->
            SPush Warp a
  asWarpMap :: (a -> SPush t b)
               -> SPull a
               -> SPush Warp b


instance AsWarp Thread where
  asWarp = tConcat
  asWarpMap f = tConcat . fmap f

instance AsWarp Warp where
  asWarp = sConcat
  asWarpMap f = sConcat . fmap f

---------------------------------------------------------------------------
-- LiftThread
---------------------------------------------------------------------------

asThread :: ASize l
           => Pull l (SPush Thread b)
           -> Push Thread l b
asThread = tConcat

asThreadMap :: (a -> SPush Thread b)
               -> SPull a
               -> SPush Thread b
asThreadMap f = tConcat . fmap f


---------------------------------------------------------------------------
-- AsGrid
---------------------------------------------------------------------------

asGrid :: ASize l => Pull l (SPush Block a)
         -> Push Grid l a
asGrid = pConcat

asGridMap :: ASize l => (a -> SPush Block b)
            -> Pull l a
            -> Push Grid l b
asGridMap f = pConcat . fmap f


---------------------------------------------------------------------------
-- Repeat a program
---------------------------------------------------------------------------

-- | Repeat a program (iterate it)
rep :: Word32 -> (a -> Program t a) -> a -> Program t a
rep 0 _ a = return a
rep n prg a = do
  b <- rep (n-1) prg a
  prg b

---------------------------------------------------------------------------
-- RunPush
---------------------------------------------------------------------------

class ExecProgram t a  where
  exec :: Data e
        => Program t (a Word32 e)
        -> Push t Word32 e

instance (t *<=* Block) => ExecProgram t Pull where
  exec = runPush . liftM push

-- Here we also want the type error behaviour.
-- It is a type error to try to "execute" a push t at any level different from t
instance (t ~ t1) => ExecProgram t (Push t1) where
  exec = runPush

execThread :: (ExecProgram Thread a, Data e)
        => Program Thread (a Word32 e)
        -> Push Thread Word32 e
execThread = exec

execThread' :: Data a => Program Thread a -> SPush Thread a
execThread' = singletonPush

execBlock :: (ExecProgram Block a, Data e)
          => Program Block (a Word32 e)
          -> Push Block Word32 e
execBlock = exec

execBlock' :: Data a => Program Block a -> SPush Block a
execBlock' = singletonPush

execWarp :: (ExecProgram Warp a, Data e)
         => Program Warp (a Word32 e)
         -> Push Warp Word32 e
execWarp = exec

execWarp' :: Data a => Program Warp a -> SPush Warp a
execWarp' = singletonPush


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
runPull :: (t *<=* Block, ASize s) => Program t (Pull s a) -> Push t s a
runPull = runPush . liftM push

-- | Lifts @runPull@ to one input functions.
runPull1 :: (t *<=* Block, ASize s) => (a -> Program t (Pull s b)) -> a -> Push t s b
runPull1 f a = runPull (f a)

-- | Lifts @runPull@ to two input functions.
runPull2 :: (t *<=* Block, ASize s) => (a -> b -> Program t (Pull s c)) -> a -> b -> Push t s c
runPull2 f a b = runPull (f a b)

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
pushPrg :: (t *<=* Block) => Program t a -> SPush t a
pushPrg = singletonPush


---------------------------------------------------------------------------
-- Singleton push arrays
---------------------------------------------------------------------------

-- Danger! use only with Scalar a's
-- -- | Create a singleton Push array.
--singletonPush :: a -> SPush t a
--singletonPush = singletonPushP . return

-- | Monadic version of @singleton@.
singletonPush :: (t *<=* Block) => Program t a -> SPush t a
singletonPush prg =
  mkPush 1 $ \wf -> do
    a <- prg
    forAll 1 $ \_ ->
      wf a 0

---------------------------------------------------------------------------
-- Old stuff that should nolonger be exported!
--  * It is still used internally
---------------------------------------------------------------------------

-- | A way to enter into the hierarchy
-- A bunch of Thread computations, spread across the threads of either
-- a Warp, block or grid. (or performed sequentially in a single thread)
tConcat :: (t *<=* Block, ASize l)
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
tDistribute :: (t *<=* Block, ASize l)
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
pDistribute :: ASize l
               => l
               -> (EWord32 -> SPush t a)
               -> Push (Step t) l a
pDistribute n f = pConcat (mkPull n f)

-- | Sequential concatenation of a Pull of Push.
sConcat :: ASize l => Pull l (SPush t a) -> Push t l a
sConcat arr =
  mkPush (n * fromIntegral rn) $ \wf ->
  do
    seqFor (sizeConv n) $ \bix ->
      let p = arr ! bix
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
pUnCoalesce :: ASize l
               => Pull l (SPush t a)
               -> Push (Step t) l a
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

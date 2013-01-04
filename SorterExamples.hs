{-# LANGUAGE ScopedTypeVariables,
             FlexibleContexts #-} 
             
module SorterExamples where

-- import qualified Foreign.CUDA.Driver as CUDA
-- import qualified Foreign.CUDA.Driver.Device as CUDA
import qualified Obsidian.CodeGen.CUDA as CUDA
-- import Obsidian.Run.CUDA.Exec

import Obsidian.Program
import Obsidian.Exp
import Obsidian.Types
import Obsidian.Array
import Obsidian.Library
import Obsidian.Force

import Data.Word
import Data.Int
import Data.Bits

import qualified Data.Vector.Storable as V

import Control.Monad.State

import Prelude hiding (zipWith,sum,replicate)
import qualified Prelude as P 

---------------------------------------------------------------------------
-- mapD experiments
---------------------------------------------------------------------------

mapD :: (a -> BProgram b) ->
        (Distrib a -> Distrib (BProgram b))
mapD f inp@(Distrib nb bixf) =
  Distrib nb $ \bid -> f (bixf bid)




-- first, bitonic merge


bmergeL :: Int -> [Pull EInt -> Pull EInt]
bmergeL n = [stage (n-i) | i <- [1..n]]
  where
    stage i = ilv1 i min max

bmerge :: Int
            -> Distrib (Pull EInt)
            -> Distrib (BProgram (Pull EInt))
bmerge logbsize arr =
  mapD (compose (bmergeL logbsize)) arr

tmergeL :: Int -> [Pull EInt -> Pull EInt]
tmergeL n = vstage (n-1) : [istage (n-i) | i <- [2..n]]
  where
    vstage i = vee1 i min max
    istage i = ilv1 i min max


tmerge :: Int
            -> Distrib (Pull EInt)
            -> Distrib (BProgram (Pull EInt))
tmerge logbsize arr =
  mapD (compose (tmergeL logbsize)) arr

-- Because tmerge sorts two half-length sorted lists, it is easy to compose a tree of them to make a sorter (tsort1)

tsort1L :: Int -> [Pull EInt -> Pull EInt]
tsort1L n = concat [tmergeL i | i <- [1..n]]

tsort1 :: Int
            -> Distrib (Pull EInt)
            -> Distrib (BProgram (Pull EInt))
tsort1 logbsize arr =
  mapD (compose (tsort1L logbsize)) arr



tmerge2L :: Int -> [Pull EInt -> Push EInt]
tmerge2L n = vee2 (n-1) min max : [(ilv2 (n-i) min max)| i <- [2..n]]

-- tmerge :: Int
--             -> Distrib (Pull EInt)
--            -> Distrib (BProgram (Pull EInt))
tmerge2 logbsize arr =
  mapD (compose (tmerge2L logbsize)) arr


-- compose :: [Pull EInt -> Pull EInt] -> Pull EInt -> BProgram (Pull EInt)
compose [] arr = return arr
compose (f:fs) arr = 
  do
    let arr1 = f arr
    arr2 <- force arr1
    compose fs arr2
    



ilv1 :: Choice a => Int -> (b -> b-> a) -> (b -> b -> a) -> Pull b -> Pull a
ilv1 i f g arr = mkPullArray (len arr) ixf 
  where
    ixf ix = let l = arr ! ix
                 r = arr ! newix
                 newix = flipBit i ix
             in ifThenElse (lowBit i ix) (f l r) (g l r)

vee1 :: Choice a => Int -> (b -> b-> a) -> (b -> b -> a) -> Pull b -> Pull a
vee1 i f g arr = mkPullArray (len arr) ixf 
  where
    ixf ix = let l = arr ! ix
                 r = arr ! newix
                 newix = flipLSBsTo i ix
             in ifThenElse (lowBit i ix) (f l r) (g l r)



ilv2 :: Choice b => Int -> (a -> a -> b) -> (a -> a -> b) -> Pull a -> Push b
ilv2 i f g arr 
   = Push n $ \wf -> a5 wf >> a6 wf
  where
    n  = len arr
    n2 = n `div` 2
    a1 = resize (n-n2) (ixMap left arr) -- Take (n-n2) elements from 'left'permuted arr
    a2 = resize n2 (ixMap right arr)    -- Take n2 elements from 'right'permuted arr
    a3 = zipWith f a1 a2
    a4 = zipWith g a1 a2                -- up to here we are still in pull arrays
    (Push _ a5) = ixMap left (push a3)
    (Push _ a6) = ixMap right (push a4) -- here same permutation is applied..
                                        -- What does that mean ?
    left = insertZero i
    right = flipBit i  . left

---------------------------------------------------------------------------
-- Spliting Ilv up. Just to see if it leads to anything.
---------------------------------------------------------------------------

-- What does the i represent ?
-- This one always splits into 2 parts. Is 'i' concerned with that ?
-- What are the allowed i's ? (0 to numbits ?) 
ilvPermute :: Int -> Pull a -> (Pull a, Pull a)
ilvPermute i arr =
  (a1, a2)
  where
    n  = len arr
    n2 = n `div` 2 -- Here div by 2 (center split).
                   -- Does all i's lead to a division into equal parts ?
    -- Pull out the elements we are interested in. 
    a1 = resize (n-n2) (ixMap left arr) 
    a2 = resize n2 (ixMap right arr)    
    left = insertZero i
    right = flipBit i  . left

ilvPush :: Int -> Pull a -> Pull a -> Push a
ilvPush i a1 a2 =
  Push n $ \wf -> pf1 wf >> pf2 wf
  where
    -- Push to correct position. I do not quite get this part
    -- It seems to me then that ixMap means two different things
    -- related to if it acts on push or pull array.
    (Push _ pf1) = ixMap left (push a1)
    (Push _ pf2) = ixMap right (push a1) 
    left = insertZero i
    right = flipBit i  . left
            

ilv2' i f g arr = ilvPush i a1 a2 
  where
    (a1,a2) = ilvPermute i arr
    a3 = zipWith f a1 a2
    a4 = zipWith g a1 a2


---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------

-- Here is where I am stuck. I want to make a global ilv
-- with roughly this type. Perhaps the inputs need to be tupled?

ilvG :: Exp Int -> Distrib (Pull (Exp Int))-> GlobArray (Exp Int)
ilvG i distr@(Distrib nb bixf) =
  GPush nb bs $
    \wf ->
       do
         ForAllBlocks nb $
           \bix -> ForAll bs $
                   \tix -> undefined
  where 
    bs = len (bixf 0) 

{--  This is the CUDA that I want to generate in the end (probably
with stride as first input) :

__global__ void iSwap(
    int *d_input,
    int *d_output,
    unsigned int stride){

    unsigned int tid = blockIdx.x * blockDim.x + threadIdx.x;
   
    unsigned int ix = tid + (tid & ~(stride - 1));

    int v1 = d_input[ix];
    int v2 = d_input[ix + stride];

    d_output[ix] = min(v1,v2);
    d_output[ix + stride] = max(v1,v2);
    
}

--}


                   

vee2 :: Choice b => Int -> (a -> a -> b) -> (a -> a -> b) -> Pull a -> Push b
vee2 i f g arr  
   = Push n $ \wf -> a5 wf >> a6 wf 
  where
    n  = len arr
    n2 = n `div` 2
    a1 = resize (n-n2) (ixMap left arr)  -- Array (Pull (ixf . left)) (n-n2)
    a2 = resize n2 (ixMap right arr)    -- Array (Pull (ixf . right)) n2
    a3 = zipWith f a1 a2
    a4 = zipWith g a1 a2
    (Push _ a5) = ixMap left (push a3)
    (Push _ a6) = ixMap right (push a4)
    left = insertZero i
    right = flipLSBsTo i  . left



-- bit manipulations. In prep. for the global kernels that I want, I started
-- to add some that only work on Exp inputs.

flipBit :: Bits a => Int -> a -> a
flipBit = flip complementBit 

fBit :: Bits a => EInt -> a -> a
fBit i a = a `xor` (2^i)

lowBit :: Int -> EWord32 -> Exp Bool
lowBit i ix = (ix .&. bit i) ==* 0

oneBits :: Bits a => Int -> a
oneBits i = bit i - 1


flipLSBsTo :: Int -> EWord32 -> EWord32
flipLSBsTo i = (`xor` (oneBits (i+1)))

insertZero :: Int -> Exp Word32 -> Exp Word32
insertZero 0 a = a `shiftL` 1
insertZero i a = a + (a .&. fromIntegral (complement (oneBits i :: Word32)))

iZero :: Exp Int32 -> Exp Word32 -> Exp Word32
iZero i a = a + (a .&. complement (oBits i))

oBits :: Bits a => Exp Int32 -> a
oBits i = (2^i) - 1



-- testing

input5 :: Int -> Word32 -> Distrib (Pull (Exp Int))
input5 k n = namedGlobal "apa" (2^k) (2^n)

writegen k n s f = 
  writeFile (s ++ ".cu") $ CUDA.genKernel s 
   (cheat . forceG . toGlobArrayN 2 . f k) (input5 k n)

-- comment. Probably the toGlobArrayN 2 should not work on adjacent elements

runt2 k n = writegen k n "tmerge2" tmerge2
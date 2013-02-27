{-# LANGUAGE ScopedTypeVariables,
             FlexibleContexts,
             GADTs #-} 

module Examples where

import qualified Obsidian.CodeGen.CUDA as CUDA

import Obsidian.Program
import Obsidian.Exp
import Obsidian.Types
import Obsidian.Array
import Obsidian.Library
import Obsidian.Force
import Obsidian.CodeGen.InOut
import Obsidian.Atomic

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

splitUp :: Word32 -> Pull (Exp Word32) a -> Pull (Exp Word32) (Pull Word32 a)
splitUp n (Pull m ixf) = Pull (m `div` fromIntegral n) $ 
                          \i -> Pull n $ \j -> ixf (i * (fromIntegral n) + j)

test1 :: Pull (Exp Word32) EInt -> GProgram (Push Grid (Exp Word32) EInt)
test1 input = computeBlocks $ fmap mapFusion (splitUp 256 input) 

test2 :: Pull (Exp Word32) EInt -> GProgram () -- Push Grid (Exp Word32) EInt
test2 input = forceG $ computeBlocks' $ fmap mapFusion (splitUp 256 input) 

input1 :: Pull (Exp Word32) EInt 
input1 = namedGlobal "apa" (variable "X")


---------------------------------------------------------------------------
-- WORK IN PROGRESS
--------------------------------------------------------------------------- 
computeBlocks :: forall a . StoreOps a
                 => Pull (Exp Word32) (BProgram (Pull Word32 a)) ->
                 -- GProgram enables sharing...
                 -- If that was not needed just go to Push. 
                 GProgram (Push Grid (Exp Word32) a) 
computeBlocks (Pull bs bxf) =
  do
    let arr = fst $ runPrg 0 $ bxf 0
        n   = len arr

    sm <- names (undefined :: a) 
    allocate sm (undefined :: a) n
                
    
    ForAllBlocks bs $ \bix ->
      do
        arr <- bxf bix -- Perform the local computation
        let (Push n p) = push Block arr
            
        -- Extra - possibly unnecessary - store

        p (assign sm) 

    -- Create a slightly special pull array     
    let pully = Pull (bs * sizeConv n)
                   $ \gix -> (pullFrom sm n) ! (gix `mod` fromIntegral n) 
    return $ push Grid pully 


computeBlocks' :: forall a . StoreOps a
                   => Pull (Exp Word32) (BProgram (Pull Word32 a)) ->
                   Push Grid (Exp Word32) a
computeBlocks' (Pull bs bxf) =
  Push (bs * (fromIntegral n)) $ \ wf -> 
    ForAllBlocks bs $ \bix ->
      do
        arr <- bxf bix 
        ForAll (fromIntegral n) $ \tix ->
          wf (arr ! tix) (bix * fromIntegral (len arr) + tix) 
      
  where
    arr = fst $ runPrg 0 $ bxf 0
    n  = len arr


---------------------------------------------------------------------------
-- Scans 
---------------------------------------------------------------------------
sklansky :: (Choice a, StoreOps a)
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


-- Many local Scans
--sklanskyG :: (Choice a, StoreOps a)
--             => Int -> (a -> a -> a)
--             -> Pull (Exp Word32) a
--            -> Push Grid (Exp Word32) a
sklanskyG logbs op =
  forceG . computeBlocks' . fmap (sklansky logbs op) . splitUp (2^logbs) 

getSklansky =
  quickPrint (sklanskyG 8 (+))
             (undefinedGlobal (variable "X") :: Pull (Exp Word32) EInt32)

---------------------------------------------------------------------------
-- kStone (TEST THAT THIS IS REALLY A SCAN!) 
---------------------------------------------------------------------------
kStone :: (Choice a, StoreOps a) 
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
kStoneP :: (Choice a, StoreOps a) 
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
  forceG . computeBlocks' . fmap (kStone logbs op) . splitUp (2^logbs)
kStonePG logbs op =
  forceG . computeBlocks' . fmap (kStoneP logbs op) . splitUp (2^logbs) 

getKStone =
  quickPrint (kStoneG 8 (+))
             (undefinedGlobal (variable "X") :: Pull (Exp Word32) EInt32)

getKStoneP =
  quickPrint (kStonePG 8 (+))
             (undefinedGlobal (variable "X") :: Pull (Exp Word32) EInt32)

---------------------------------------------------------------------------
-- Brent Kung
--------------------------------------------------------------------------- 


-- Incorrect! (and spots a bug somewhere)
bKung :: (Choice a, StoreOps a) 
         => (a -> a -> a) -> Pull Word32 a -> BProgram (Pull Word32 a)
bKung op arr | len arr == 1 = return arr
bKung op arr =
  do
    r1 <- force (evens arr)
    r2 <- force (evens arr) 
    --r1 <- before arr
    --r4 <- force (evens arr) 
    --r2 <- bKung op r1
    --r3 <- force$ shuffle Block r4 r2
    force $ shuffle Block r1 r2

  where
    before input =
      do 
        let r8 = zipWith op (evens input) (odds input)
        force$ r8 -- shuffle Block (evens arr) r1 

      
    -- after  arr = zipWith op (drop 1 arr) (drop 2 arr) 



bKungG op =
  forceG . computeBlocks' . fmap (bKung op) . splitUp 256

getBKung =
  quickPrint (bKungG (+))
             (undefinedGlobal (variable "X") :: Pull (Exp Word32) EInt32)


---------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------- 







{- 
input1 :: Pull EInt 
input1 = namedArray "apa" 32

input2 :: GlobPull EInt
input2 = namedGlobal "apa" 

input3 :: GlobPull (Exp Int32)
input3 = namedGlobal "apa" 



---------------------------------------------------------------------------
-- Small experiments 
---------------------------------------------------------------------------

sync :: (Len p, Pushable p, StoreOps a) => p a -> BProgram (Pull a)
sync = force 

prg0 = putStrLn$ printPrg$  mapFusion input1

mapFusion' :: GlobPull EInt
              -> GlobPush EInt
mapFusion' arr = mapG mapFusion 256 arr
                 
prg1 = putStrLn$ printPrg$ cheat $ (forceG . mapFusion') input2


---------------------------------------------------------------------------
-- Permutations
--  Data dependent permutations
--------------------------------------------------------------------------- 
permutePush :: GlobPull (Exp Word32)
               -> GlobPull a
               -> GlobPush a
permutePush perm dat@(GlobPull gixf) = 
  GlobPush $
    \wf -> -- (a -> W32 -> TProgram)
         forAllT $
           \gix ->
            let gix' = perm ! gix
            in  wf (gixf gix) gix' 

      
perm :: GlobPull (Exp Word32)
        -> GlobPush a
        -> GlobPush a
perm perm@(GlobPull pf) (GlobPush pushf) =
  GlobPush $
  \wf -> -- (a -> W32 -> TProgram)
   pushf (\a gix -> wf a (pf gix)) 

 
---------------------------------------------------------------------------
--
-- Countingsort start
--
---------------------------------------------------------------------------

-- gather: output is as long as the array of indices.
--   this is same as permutePush above (gather may be better name) 
gatherGlobal :: GlobPull (Exp Word32)
                -> GlobPull a
                -> GlobPush a
gatherGlobal indices elems    =
  GlobPush $
  \wf ->
    forAllT $ \gix -> let inix = indices ! gix
                          e    = elems ! inix 
                      in wf e gix 

scatterGlobal :: GlobPull (Exp Word32) -- where to scatter
                 -> GlobPull a -- the elements to scatter
                 -> GlobPush a 
scatterGlobal indices elems =
  GlobPush $
    \wf -> forAllT $ \gix ->
    let scix = indices ! gix
        e    = elems   ! gix
    in  wf e scix 

distribute :: a -> GlobPull a
distribute e = GlobPull $ \gix -> e           

histogram :: GlobPull (Exp Int32)
             -> GlobPush (Exp Int32)
             -- a type cast is needed!
histogram elems = scatterGlobal (fmap int32ToWord32 elems) (distribute 1)


reconstruct :: GlobPull (Exp Int32)
               -> GlobPull (Exp Word32)
               -> GlobPush (Exp Int32)
reconstruct inp pos =
  gatherGlobal perm inp  
  where
    perm =
      GlobPull $ \gix ->
      let gix' = (fmap int32ToWord32 inp) ! gix 
      in  pos ! gix' 

-- This function is supposed to compute the histogram for the input vector.
-- Three things I'm not sure about:
    -- 1. The looping that I do seems to be wrong. How do I do it correctly?
    -- The intention is to loop over the whole input array.
    -- 2. Are arrays initialized to zero? If we want to use this for counting
    -- sort then the `global` array needs to be zero everywhere from the start.
    -- 3. The type is really weird. It uses both GlobPull and GlobPull2. I just
    -- used whatever was most convenient for each task.
fullHistogram :: GlobPull (Exp Int32)
                 -> Final (GProgram (GlobPull (Exp Int)))
fullHistogram (GlobPull ixf) = Final $
                 do global <- Output $ Pointer (typeOf (undefined :: Exp Int32))
                    forAllT $ \gix ->
                      do AtomicOp global (int32ToWord32 (ixf gix))  AtomicInc
                         return ()
                    return (GlobPull (\i -> index global i))

getFullHistogram = quickPrint (fullHistogram) undefinedGlobal


-- Now this one will break code generation ! 
atomicForce :: forall a. Scalar a => Atomic a
               -> GlobPull (Exp Word32)
               -> GlobPull (Exp a)
               -> Final (GProgram (GlobPull (Exp a)))    
atomicForce atop indices dat = Final $ 
  do 
    global <- Output $ Pointer (typeOf (undefined :: Exp a))

    forAllT $ \gix ->
      Assign   global gix (dat ! gix)
    ThreadFence
    forAllT $ \gix ->
      do 
        AtomicOp global (indices ! gix) atop
        return ()
         
    return $ GlobPull (\gix -> index global gix)

      

-- Possible answers:
-- #1: I think I adapted the code to answer this.
--     This is how it might work, at least in this experimental branch
-- #2: Normally not initialized to zero. When allocating a global array
--     it should be followed by a "memset". but this is outside of what we
--     can do from inside Obsidian right now.
-- #3: I changed this to only use GlobPull. (GlobPull2 is removed in this branch)

-- Are the types supposed to be this way ? 
fullReconstruct :: GlobPull (Exp Word32)
                -> GlobPush (Exp Word32)
fullReconstruct (GlobPull ixf) = GlobPush f
  where f k = do forAllT $ \gix ->
                   let startIx = ixf gix in
                   SeqFor (ixf (gix + 1) - startIx) $ \ix ->
                      k gix (ix + startIx)

getFullReconstr = quickPrint (forceG . fullReconstruct) undefinedGlobal

---------------------------------------------------------------------------
-- Scan
---------------------------------------------------------------------------
sklansky :: (Num (Exp a), Scalar a)
            => Int
            -> (Exp a -> Exp a -> Exp a)
            -> Pull (Exp a)
            -> BProgram (Pull (Exp a))
sklansky 0 op arr = return arr
sklansky n op arr =
  do 
    let arr1 = twoK (n-1) (fan op) arr
    arr2 <- force arr1
    sklansky (n-1) op arr2


sklanskyT :: (Choice a, Num a, StoreOps a)
            => Int
            -> (a -> a -> a)
            -> Pull a
            -> BProgram (Pull a)
sklanskyT 0 op arr = return arr
sklanskyT n op arr =
  do 
    let arr1 = twoK (n-1) (fan op) arr
    arr2 <- force arr1
    sklanskyT (n-1) op arr2


cons :: Choice a => a -> Pull a -> Pull a
cons a p = singleton a `conc` p 

sklanskyMax n op arr = do
  res <- sklansky n op arr
  let m = fromIntegral $ len res -1 
  return (res,singleton (res ! m))

sklanskyMaxT n op arr = do
  res <- sklanskyT n op arr
  let m = fromIntegral $ len res -1 
  return (res,singleton (res ! m))


-- Need to add codegen support form
-- functions returning GPrograms !
-- Codegeneration now also may need to support
-- certain cases of sequences of ForAllBlock..
-- I Think the important distinction there is if
-- its used as a case of forAllT or not. Maybe this
-- indicates that there should be a separate ForAllT
-- constructor in the Program type. 
sklanskyG :: (Num (Exp a), Scalar a)
             => Int
             -> GlobPull (Exp a)
             -> GProgram (GlobPush  (Exp a), GlobPush (Exp a))
sklanskyG logbsize input =
  toGProgram (mapDist (sklanskyMax logbsize (+)) (2^logbsize) input)

sklanskyGT ::Int -> GlobPull (Exp Word32)
             -> GProgram (GlobPush (Exp Word32), GlobPush (Exp Word32))
sklanskyGT logbsize input =
  toGProgram (mapDist (sklanskyMaxT logbsize (+)) (2^logbsize) input)


sklanskyGP logbsize input =
  do
    (r1,r2) <- sklanskyG logbsize input
    forceGP r1
    forceGP r2
    return (r1, r2)

sklanskyGPT logbsize input =
  do
    (r1,r2) <- sklanskyGT logbsize input
    forceGP r1
    forceGP r2
    return (r1, r2)


getSklanskyG = quickPrint (sklanskyG 8) (undefinedGlobal :: GlobPull (Exp Int))
getSklanskyGP = quickPrint (sklanskyGP 8) (undefinedGlobal :: GlobPull (Exp Int))

getSklanskyGPT = quickPrint (sklanskyGPT 8) (undefinedGlobal :: GlobPull EWord32)

fan op arr =  a1 `conc`  fmap (op c) a2 
    where 
      (a1,a2) = halve arr
      c = a1 ! (fromIntegral (len a1 - 1))




sklanskyAllBlocks :: Int
                     -> GlobPull (Exp Int32)
                     -> GlobPush (Exp Int32)
sklanskyAllBlocks logbsize arr =
  mapG (sklansky logbsize (+)) (2^logbsize) arr

{- 
blockReplicate :: Word32 -- blockSize
                   -> Pull (Exp Word32)
                   -> Distrib (Pull (Exp Word32))
blockReplicate bs inp =
  Distrib newPull
    where
      mi = fromIntegral bs - 1
      newPull bix = Pull bs $ \ix -> inp ! bix

---------------------------------------------------------------------------
-- Print Kernels
---------------------------------------------------------------------------
-} 
getHist = quickPrint (forceG .  histogram) undefinedGlobal

getRecon = quickPrint reconstruct'  
             ((undefinedGlobal :: GlobPull (Exp Int32)) :->
              (undefinedGlobal :: GlobPull (Exp Word32)))
           where
             reconstruct' i1 i2 = forceG (reconstruct i1 i2)

getSklansky = quickPrint (forceG . sklanskyAllBlocks 8)
                         undefinedGlobal


{- 
---------------------------------------------------------------------------
--
-- Experimenting with GlobPull and GlobPull2.
--  These represent two different "views" on a global array. 
--  GlobPull pulls from a Global array using a global thread id
--   (bix * bs + ix usually).
--  GlobPull2 pulls from a Global array using a block id and a thread id.
--
--  Maybe GlobPull2 can replace Distrib.
--
--  Hopefully GlobPull provides a cleaner way to implement simple global
--  permutation.
--
--  I have a feeling there is a generalisation of these. Let's see
--  if it surfaces. 
---------------------------------------------------------------------------
-}

-- What if global arrays do not have a blocksize associated with them
-- mapGP :: Word32
--          -> (Pull a -> BProgram (Pull b))
--          -> GlobPull a
--          -> GlobPush b
-- mapGP n f (GlobPull _ ixf)  =
--   GlobPush n
--         $ \wf ->
--           ForAllBlocks 
--            $ \bix ->
--              do -- BProgram do block 
--                let pully = Pull n (\ix -> ixf (bix * fromIntegral n + ix))
--                res <- f pully
--                -- Here a number is needed. 
--                ForAll n $ \ix -> wf (res ! ix) (bix * fromIntegral n + ix)
               



{- 
---------------------------------------------------------------------------
-- Global computations may care about number of blocks!
---------------------------------------------------------------------------

-- The number of blocks is rarely used.
-- But here in reverseG the nblocks is needed. Maybe such
-- functions where the number of blocks are needed should take that value
-- as input? This means the representation of Global arrays does not need to carry
-- that value along with them (at all). 
reverseG :: Exp Word32 -> GlobPull a -> GlobPull a
reverseG bs (GlobPull n ixf) =  GlobPull n (\ix -> ixf (bs * (fromIntegral n) - ix - 1))



---------------------------------------------------------------------------
-- Testing new kinds of Arrays 
---------------------------------------------------------------------------
sklanskyAllBlocks' :: Int
                     -> GlobPull (Exp Word32)
                     -> GlobPush (Exp Word32)
sklanskyAllBlocks' logbsize arr =
  mapG (sklanskyLocal logbsize (+)) arr

-- (changeIn . silly) is just there until a proper InOut instance is
-- created for GlobPull arrays.
getSklansky' = quickPrint (forceG . sklanskyAllBlocks' 8 . changeIn . silly)
                          (sizedGlobal 256)
{-
   Pros: Compared to the Distrib version, lots simpler types (cleaner).
   Cons: Not sure. Maybe less flexible ?
-} 

---------------------------------------------------------------------------
-- Push Experiments
---------------------------------------------------------------------------

pushBy :: [Exp Word32 -> Exp Word32] -> Pull a -> Push a
pushBy ixtrans (Pull n ixf) =
  Push n 
  $ \wf ->  ForAll (n `div` fromIntegral m)
            $ \ix -> sequence_ [wf (ixf i) ((ixtrans !! j) i)
                               | j <- [0..m-1]
                               , let i = ix * fromIntegral m +
                                              fromIntegral j]
                     
  where
    m = length ixtrans

pushByP :: (Exp Word32 -> Exp Word32,
            Exp Word32 -> Exp Word32)
           -> Pull (a,a)
           -> Push a
pushByP (t1,t2) (Pull n ixf) =
  Push (n*2)
  $ \wf -> ForAll n
           $ \ix -> sequence_ [wf (fst (ixf ix)) (t1 ix),
                               wf (snd (ixf ix)) (t2 ix)]


---------------------------------------------------------------------------
-- pushBy test
---------------------------------------------------------------------------

testBy :: Pull (Exp Int32) -> Push (Exp Int32)
testBy = pushBy [(`div` 2),\x -> x `div` 2 + 1 ]


  
testAB :: GlobPull (Exp Int32)
          -> GlobPush (Exp Int32)
testAB = mapG (force . testBy)

getTestAB = quickPrint (forceG . testAB . changeIn . silly)
                          (sizedGlobal 256)



---------------------------------------------------------------------------
-- Apply an n-input m-output sequential computation across in parallel
-- across an array
---------------------------------------------------------------------------
-} 
mapSeq :: ([a] -> [b]) -> Pull [a] -> Push b
mapSeq f (Pull bs ixf) =
  Push (bs * fromIntegral n)
  $ \wf -> ForAll (Just bs)
           $ \ ix ->
           let dat = f (ixf ix) 
               m   = length dat
           in sequence_ [wf (dat !! i) (ix * fromIntegral m + fromIntegral i)
                        | i <- [0..m-1]]
  where
    n = length (ixf 0) -- in an array of lists all list have same length.

{- Intended use of mapSeq:

   #1 create a n-input m-output function

   ex:  f [a,b] = [min a b, max a b]

   #2 permute input pull array in whatever way you want

   #3 split input pull array up into an array of lists

   #4 mapSeq f over the array

   #5 permute resulting push array in whatever way you want.

   --

   mapSeq could have been given the type ([a] -> [b]) -> Pull [a] -> Pull [b]
      
-} 
    
chunk :: Int -> Pull a -> Pull [a]
chunk cs (Pull n ixf) =
  Pull (n `div` fromIntegral cs)
  $ \ix -> [ixf (ix * fromIntegral cs + fromIntegral i)
           | i <- [0..cs-1]]

---------------------------------------------------------------------------
-- Maybe what Mary needs. (Here in the simpler local array version) 
---------------------------------------------------------------------------
mapPermSeq :: ([a] -> [b])
              -> (Exp Word32 -> [Exp Word32]) -- Not nice! 
              -> (Exp Word32 -> [Exp Word32]) -- Not nice!
                                               
              -> Pull a -> Push b
mapPermSeq f inp outp pull@(Pull bs ixf) =
  
  Push (bn * fromIntegral outN)
  $ \wf -> ForAll (Just bn)
           $ \ix ->
           let p   = gatherSeq pull
               dat = f (p ! ix)  -- apply sequential computation
           in  sequence_ [wf (dat !! i) ((outp ix) !! i)
                         | i <- [0..outN-1]]
           
  where
    -- create a Pull [a] with help of the inP

    bn = bs `div` fromIntegral inN 
    gatherSeq (Pull n ixf) =
      Pull (n `div` fromIntegral inN)
      $ \ix -> [ixf i | i <- inp ix]
               
    inN = length (inp (variable "X")) 
    outN = length (outp (variable "X"))
   

---------------------------------------------------------------------------
-- And Again for Global arrays.
--
-- There should be a way to unify these. 
---------------------------------------------------------------------------
mapPermSeqG :: ([a] -> [b])
               -> (Exp Word32 -> [Exp Word32]) -- Not Nice! 
               -> (Exp Word32 -> [Exp Word32]) -- Not Nice!
               -> GlobPull a -> GlobPush b
mapPermSeqG f inp outp pull@(GlobPull ixf) =
  -- Here I start to get very confused about what will happen
  -- when using forAllT (and its direct usage of CUDA's BlockDim.x) 
  GlobPush 
  $ \wf -> forAllT
           $ \gix -> let p = gatherSeq pull
                         dat = f (p ! gix)  
                     in sequence_ [wf (dat !! i) ((outp gix) !! i) 
                                  | i <- [0..outN-1]]
  where 
    gatherSeq (GlobPull ixf) =
      GlobPull $ \ix -> [ixf i | i <- inp ix]
               
    inN = length (inp (variable "X")) 
    outN = length (outp (variable "X"))

---------------------------------------------------------------------------
-- Testing 
---------------------------------------------------------------------------

test3 :: GlobPull (Exp Int32) -> GlobPush (Exp Int32)
test3 = mapPermSeqG (\[a,b] -> [min a b, max a b])
                    (\ix -> [ix, ix + 1024])
                    (\ix -> [ix, ix + 1024])



  
-- getTest3 and getTest3' should give same code

getTest3 = quickPrint (forceG . test3 ) undefinedGlobal



-- TODO: Probably lots of bugs right now


---------------------------------------------------------------------------
-- Hacking
---------------------------------------------------------------------------
forAllT' :: GlobPull (Program Thread ()) -> Program Grid ()
forAllT' (GlobPull gixf) = forAllT gixf

forAllLocal :: Pull (Program Thread ()) -> Program Block ()
forAllLocal (Pull n ixf) = ForAll (Just n) ixf 
-} 


{- 
__global__ void kernel(int32_t *input0,int32_t *output0){
  
  extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];
  ((int32_t *)sbase)[threadIdx.x] = ((threadIdx.x<2) ? input0[((blockIdx.x*256)+threadIdx.x)] : (input0[((blockIdx.x*256)+(threadIdx.x-2))]+input0[((blockIdx.x*256)+threadIdx.x)]));
  __syncthreads();
  ((int32_t *)(sbase + 1024))[threadIdx.x] = ((threadIdx.x<4) ? ((int32_t *)sbase)[threadIdx.x] : (((int32_t *)sbase)[(threadIdx.x-4)]+((int32_t *)sbase)[threadIdx.x]));
  __syncthreads();
  ((int32_t *)sbase)[threadIdx.x] = ((threadIdx.x<8) ? ((int32_t *)(sbase+1024))[threadIdx.x] : (((int32_t *)(sbase+1024))[(threadIdx.x-8)]+((int32_t *)(sbase+1024))[threadIdx.x]));
  __syncthreads();
  ((int32_t *)(sbase + 1024))[threadIdx.x] = ((threadIdx.x<16) ? ((int32_t *)sbase)[threadIdx.x] : (((int32_t *)sbase)[(threadIdx.x-16)]+((int32_t *)sbase)[threadIdx.x]));
  __syncthreads();
  ((int32_t *)sbase)[threadIdx.x] = ((threadIdx.x<32) ? ((int32_t *)(sbase+1024))[threadIdx.x] : (((int32_t *)(sbase+1024))[(threadIdx.x-32)]+((int32_t *)(sbase+1024))[threadIdx.x]));
  __syncthreads();
  ((int32_t *)(sbase + 1024))[threadIdx.x] = ((threadIdx.x<64) ? ((int32_t *)sbase)[threadIdx.x] : (((int32_t *)sbase)[(threadIdx.x-64)]+((int32_t *)sbase)[threadIdx.x]));
  __syncthreads();
  ((int32_t *)sbase)[threadIdx.x] = ((threadIdx.x<128) ? ((int32_t *)(sbase+1024))[threadIdx.x] : (((int32_t *)(sbase+1024))[(threadIdx.x-128)]+((int32_t *)(sbase+1024))[threadIdx.x]));
  __syncthreads();
  ((int32_t *)(sbase + 1024))[threadIdx.x] = ((int32_t *)sbase)[threadIdx.x];
  if (threadIdx.x<0){
    ((int32_t *)(sbase + 1024))[(256+threadIdx.x)] = (((int32_t *)sbase)[threadIdx.x]+((int32_t *)sbase)[(threadIdx.x+256)]);
    
  }
  __syncthreads();
  output0[((blockIdx.x*256)+threadIdx.x)] = ((int32_t *)(sbase+1024))[threadIdx.x];
  
}


blocks (i){
getId;
MonadReturn;
MonadReturn;
arr1 = malloc(2040);
par (i in 0..255){
arr1[i] = ( +  apa[( +  ( *  BIX 256 ) i )] apa[( +  ( *  BIX 256 ) ( +  i 1 ) )] );

}MonadReturn;
Sync;
MonadReturn;
getId;
MonadReturn;
MonadReturn;
arr3 = malloc(1016);
par (i in 0..127){
arr3[i] = ( +  arr1[( *  2 i )] arr1[( *  2 ( +  i 1 ) )] );

}MonadReturn;
Sync;
MonadReturn;
getId;
MonadReturn;
MonadReturn;
arr5 = malloc(504);
par (i in 0..63){
arr5[i] = ( +  arr3[( *  2 i )] arr3[( *  2 ( +  i 1 ) )] );

}MonadReturn;
Sync;
MonadReturn;
getId;
MonadReturn;
MonadReturn;
arr7 = malloc(248);
par (i in 0..31){
arr7[i] = ( +  arr5[( *  2 i )] arr5[( *  2 ( +  i 1 ) )] );

}MonadReturn;
Sync;
MonadReturn;
getId;
MonadReturn;
MonadReturn;
arr9 = malloc(120);
par (i in 0..15){
arr9[i] = ( +  arr7[( *  2 i )] arr7[( *  2 ( +  i 1 ) )] );

}MonadReturn;
Sync;
MonadReturn;
getId;
MonadReturn;
MonadReturn;
arr11 = malloc(56);
par (i in 0..7){
arr11[i] = ( +  arr9[( *  2 i )] arr9[( *  2 ( +  i 1 ) )] );

}MonadReturn;
Sync;
MonadReturn;
getId;
MonadReturn;
MonadReturn;
arr13 = malloc(24);
par (i in 0..3){
arr13[i] = ( +  arr11[( *  2 i )] arr11[( *  2 ( +  i 1 ) )] );

}MonadReturn;
Sync;
MonadReturn;
getId;
MonadReturn;
MonadReturn;
arr15 = malloc(8);
par (i in 0..1){
arr15[i] = ( +  arr13[( *  2 i )] arr13[( *  2 ( +  i 1 ) )] );

}MonadReturn;
Sync;
MonadReturn;
MonadReturn;
getId;
MonadReturn;
MonadReturn;
arr17 = malloc(8);
par (i in 0..0){
arr17[( *  i 2 )] = arr15[( +  ( *  2 i ) 1 )];
arr17[( +  ( *  i 2 ) 1 )] = arr15[( *  2 i )];

}MonadReturn;
Sync;
MonadReturn;
getId;
MonadReturn;
MonadReturn;
arr19 = malloc(0);
par (i in 0..0){
arr19[i] = ( +  arr15[( +  i 1 )] arr15[( +  i 2 )] );

}MonadReturn;
Sync;
MonadReturn;
getId;
MonadReturn;
MonadReturn;
arr21 = malloc(8);
par (i in 0..1){
arr21[( *  i 2 )] = arr13[( +  ( *  2 i ) 1 )];
arr21[( +  ( *  i 2 ) 1 )] = arr19[i];

}MonadReturn;
Sync;
MonadReturn;
getId;
MonadReturn;
MonadReturn;
arr23 = malloc(8);
par (i in 0..1){
arr23[i] = ( +  arr13[( +  i 1 )] arr13[( +  i 2 )] );

}MonadReturn;
Sync;
MonadReturn;
getId;
MonadReturn;
MonadReturn;
arr25 = malloc(32);
par (i in 0..3){
arr25[( *  i 2 )] = arr11[( +  ( *  2 i ) 1 )];
arr25[( +  ( *  i 2 ) 1 )] = arr23[i];

}MonadReturn;
Sync;
MonadReturn;
getId;
MonadReturn;
MonadReturn;
arr27 = malloc(40);
par (i in 0..5){
arr27[i] = ( +  arr11[( +  i 1 )] arr11[( +  i 2 )] );

}MonadReturn;
Sync;
MonadReturn;
getId;
MonadReturn;
MonadReturn;
arr29 = malloc(96);
par (i in 0..7){
arr29[( *  i 2 )] = arr9[( +  ( *  2 i ) 1 )];
arr29[( +  ( *  i 2 ) 1 )] = arr27[i];

}MonadReturn;
Sync;
MonadReturn;
getId;
MonadReturn;
MonadReturn;
arr31 = malloc(104);
par (i in 0..13){
arr31[i] = ( +  arr9[( +  i 1 )] arr9[( +  i 2 )] );

}MonadReturn;
Sync;
MonadReturn;
getId;
MonadReturn;
MonadReturn;
arr33 = malloc(224);
par (i in 0..15){
arr33[( *  i 2 )] = arr7[( +  ( *  2 i ) 1 )];
arr33[( +  ( *  i 2 ) 1 )] = arr31[i];
}
Sync;
getId;
arr35 = malloc(232);
par (i in 0..29){
arr35[i] = ( +  arr7[( +  i 1 )] arr7[( +  i 2 )] );
}
Sync;
getId;
arr37 = malloc(480);
par (i in 0..31){
arr37[( *  i 2 )] = arr5[( +  ( *  2 i ) 1 )];
arr37[( +  ( *  i 2 ) 1 )] = arr35[i];
}
Sync;
getId;
arr39 = malloc(488);
par (i in 0..61){
arr39[i] = ( +  arr5[( +  i 1 )] arr5[( +  i 2 )] );
}
Sync;
getId;
arr41 = malloc(992);
par (i in 0..63){
arr41[( *  i 2 )] = arr3[( +  ( *  2 i ) 1 )];
arr41[( +  ( *  i 2 ) 1 )] = arr39[i];
}
Sync;
getId;
arr43 = malloc(1000);
par (i in 0..125){
arr43[i] = ( +  arr3[( +  i 1 )] arr3[( +  i 2 )] );

}MonadReturn;
Sync;
getId;
arr45 = malloc(2016);
par (i in 0..127){
arr45[( *  i 2 )] = arr1[( +  ( *  2 i ) 1 )];
arr45[( +  ( *  i 2 ) 1 )] = arr43[i];
}
Sync;
getId;
arr47 = malloc(2024);
par (i in 0..253){
arr47[i] = ( +  arr1[( +  i 1 )] arr1[( +  i 2 )] );

}
Sync;

par (i in 0..253){
globalOut0[( +  ( *  BIX 253 ) i )] = arr47[i];

}
}



-} 
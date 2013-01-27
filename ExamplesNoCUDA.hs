{-# LANGUAGE ScopedTypeVariables,
             FlexibleContexts #-} 

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

import Prelude hiding (zipWith,sum,replicate)
import qualified Prelude as P 

{-
   -- TODO: Cond finns i Program. Fixa codegen.
   -- TODO: SeqFor finns i Program. Fixa codegen.
   -- Force: bry inte om generalisera nu (eller ngnsin). 
   -- Countingsort: generera kod, se att funkar.
   -- Riktig Countingsort: TODO!
   -- Genererade kernels behöver ibland ta längden av globala arrayer (antal block)
   --     som input. 
-} 

---------------------------------------------------------------------------
-- Util 
---------------------------------------------------------------------------
quickPrint :: ToProgram a b => (a -> b) -> Ips a b -> IO ()
quickPrint prg input =
  putStrLn $ CUDA.genKernel "kernel" prg input 

---------------------------------------------------------------------------
-- Scalar argument
---------------------------------------------------------------------------
scalArg :: EInt -> GlobPull EInt -> Final (GProgram (GlobPull EInt)) 
scalArg e = forceG . mapG (force . fmap (+e)) 256

getScalArg = quickPrint scalArg ((variable "X") :->
                                 undefinedGlobal)

---------------------------------------------------------------------------
-- MapFusion example
---------------------------------------------------------------------------

mapFusion :: Pull EInt -> BProgram (Pull EInt)
mapFusion arr =
  do
    imm <- sync $ (fmap (+1) . fmap (*2)) arr
    sync $ (fmap (+3) . fmap (*4)) imm 

input1 :: Pull EInt 
input1 = namedArray "apa" 32

input2 :: GlobPull EInt
input2 = namedGlobal "apa" 

input3 :: GlobPull (Exp Int32)
input3 = namedGlobal "apa" 



---------------------------------------------------------------------------
-- Small experiments 
---------------------------------------------------------------------------

sync :: Forceable a => a -> BProgram (Forced a)
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
sklansky 0 op arr = return (shiftRight 1 0 arr)
sklansky n op arr =
  do 
    let arr1 = twoK (n-1) (fan op) arr
    arr2 <- force arr1
    sklansky (n-1) op arr2


sklanskyMax n op arr = do
  res <- sklansky n op arr
  let m = fromIntegral $ (2^n) - 1
  return (res,res ! m) 

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

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
scalArg e = forceG . mapG (force . fmap (+e))

getScalArg = quickPrint scalArg ((variable "X") :->
                                 (sizedGlobal 256))

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
input2 = namedGlobal "apa"  32

input3 :: GlobPull (Exp Int32)
input3 = namedGlobal "apa"  32



---------------------------------------------------------------------------
-- Small experiments 
---------------------------------------------------------------------------

sync :: Forceable a => a -> BProgram (Forced a)
sync = force 

prg0 = putStrLn$ printPrg$  mapFusion input1

mapFusion' :: GlobPull EInt
              -> GlobPush EInt
mapFusion' arr = mapG mapFusion arr
                 
prg1 = putStrLn$ printPrg$ cheat $ (forceG . mapFusion') input2


---------------------------------------------------------------------------
-- Permutations
--  Data dependent permutations
--------------------------------------------------------------------------- 
permutePush :: GlobPull (Exp Word32)
               -> GlobPull a
               -> GlobPush a
permutePush perm dat@(GlobPull bs gixf) = 
  GlobPush bs $
    \wf -> -- (a -> W32 -> TProgram)
         ForAllBlocks $
           \bix -> ForAll bs $
                   \tix ->
                   let gix' = perm ! (bix * fromIntegral bs + tix)
                   in wf (gixf (bix * fromIntegral bs + tix)) gix'


permutePush2 :: GlobPull2 (Exp Word32,Exp Word32)
                -> GlobPull2 a
                -> GlobPush2 a
permutePush2 perm@(GlobPull2 _ pf)  dat@(GlobPull2 bs bixtixf) =
  GlobPush2 bs $
  \wf -> -- (a -> W32 -> W32 -> TProgram)
    ForAllBlocks $
    \bix -> ForAll bs $
            \tix ->
            let (bix',tix') = pf bix tix
                elt = bixtixf bix tix 
            in wf elt bix' tix' 
      

perm :: GlobPull (Exp Word32)
        -> GlobPush a
        -> GlobPush a
perm perm@(GlobPull _ pf) (GlobPush bs pushf) =
  GlobPush bs $
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
  GlobPush bs $
   \wf ->
     ForAllBlocks $ \ bid -> 
       ForAll bs $ \ tid -> 
         let  inix  = indices ! (bid * fromIntegral bs + tid) 
              e     = elems   ! inix  
         in wf e (bid * fromIntegral bs + tid) 
  where bs = len indices 

scatterGlobal :: GlobPull (Exp Word32) -- where to scatter
                 -- ->  Exp Word32 -- output size
                 -> Word32     -- block size
                 -> GlobPull a -- the elements to scatter
                 -> GlobPush a 
scatterGlobal indices bs elems = 
  GlobPush bs $  --what blocksize has the output. Vague.. 
    \wf -> ForAllBlocks $ \bid ->
    ForAll n $ \tid ->
    let  scix = indices ! (bid * fromIntegral n + tid)
         e    = elems   ! (bid * fromIntegral n + tid) 
    in wf e scix
       where
         n = len elems -- (should be same length or shorter then the 
                       --  indices array) 

distribute :: Word32 -> a -> GlobPull a
distribute bs e = GlobPull bs  $ \gix -> e           

histogram :: Word32
             -> GlobPull (Exp Word32)
             -> GlobPush (Exp Word32)
histogram bs elems = scatterGlobal elems bs (distribute bs 1)


reconstruct :: GlobPull (Exp Word32)
               -> GlobPull (Exp Word32)
               -> GlobPush (Exp Word32)
reconstruct inp pos =
  gatherGlobal perm inp  -- not sure! 
  where
    perm =
      GlobPull (len inp) $ \gix ->
      let gix' = inp ! gix 
      in  pos ! gix' 

{- 
---------------------------------------------------------------------------
-- Scan  (TODO: Rewrite as a exclusive scan (0 as first elem in result) 
---------------------------------------------------------------------------
sklanskyLocal
  :: (Num (Exp a), Scalar a) =>
     Int
     -> (Exp a -> Exp a -> Exp a)
     -> Pull (Exp a)
     -> BProgram (Pull (Exp a))
sklanskyLocal 0 op arr = return (shiftRight 1 0 arr)
sklanskyLocal n op arr =
  do 
    let arr1 = twoK (n-1) (fan op) arr
    arr2 <- force arr1
    sklanskyLocal (n-1) op arr2


sklansky
  :: (Num (Exp a), Scalar a) =>
     Int
     -> (Exp a -> Exp a -> Exp a)
     -> Pull (Exp a)
     -> BProgram (Pull (Exp a))
sklansky 0 op arr = return (shiftRight 1 0 arr)
sklansky n op arr =
  do 
    let arr1 = twoK (n-1) (fan op) arr
    arr2 <- force arr1
    sklanskyLocal (n-1) op arr2



fan op arr =  a1 `conc`  fmap (op c) a2 
    where 
      (a1,a2) = halve arr
      c = a1 ! (fromIntegral (len a1 - 1))

-- TODO: Too specific types everywhere! 

sklanskyAllBlocks :: Int
                     -> Distrib (Pull (Exp Word32))
                     -> Distrib (BProgram (Pull (Exp Word32)))
sklanskyAllBlocks logbsize arr =
  mapD (sklanskyLocal logbsize (+)) arr

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

getHist = quickPrint (forceG .  histogram 256) (sizedGlobal 256)

getRecon = quickPrint reconstruct'  
             ((sizedGlobal 256 :: DistArray (Exp Word32)) :-> 
              (sizedGlobal 256 :: DistArray (Exp Word32)))
           where
             reconstruct' i1 i2 = forceG (reconstruct i1 i2)

getSklansky = quickPrint (forceG . toGlobPush . sklanskyAllBlocks 8)
                         (sizedGlobal 256)



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
mapGP :: Word32
         -> (Pull a -> BProgram (Pull b))
         -> GlobPull a
         -> GlobPush b
mapGP n f (GlobPull _ ixf)  =
  GlobPush n
        $ \wf ->
          ForAllBlocks 
           $ \bix ->
             do -- BProgram do block 
               let pully = Pull n (\ix -> ixf (bix * fromIntegral n + ix))
               res <- f pully
               -- Here a number is needed. 
               ForAll n $ \ix -> wf (res ! ix) (bix * fromIntegral n + ix)
               


mapG :: (Pull a -> BProgram (Pull b))
        -> GlobPull a
        -> GlobPush b
mapG f (GlobPull n ixf)  =
  GlobPush 
        n
        $ \wf ->
          ForAllBlocks 
           $ \bix ->
             do -- BProgram do block 
               let pully = Pull n (\ix -> ixf (bix * fromIntegral n + ix))
               res <- f pully
               ForAll n $ \ix -> wf (res ! ix) (bix * fromIntegral n + ix)

mapG2 :: (Pull a -> BProgram (Pull b))
         -> GlobPull2 a
         -> GlobPush2 b
mapG2 f (GlobPull2 n bixixf) =
  GlobPush2 n
  $ \wf -> ForAllBlocks
           $ \bix ->
           do -- BProgram do block
             let pully = Pull n (\ix -> bixixf bix ix)
             res <- f pully
             ForAll n $ \ix -> wf (res ! ix) bix ix 



---------------------------------------------------------------------------
-- Is it possible to change view?
---------------------------------------------------------------------------
changeOut :: GlobPull a -> GlobPull2 a
changeOut (GlobPull n ixf) =
  GlobPull2 n $ \bix ix -> ixf (bix * (fromIntegral n) + ix)

changeIn :: GlobPull2 a -> GlobPull a
changeIn (GlobPull2 n bixixf) =
  GlobPull n $ \ gix ->
  let bix = gix `div` (fromIntegral n)
      ix  = gix `mod` (fromIntegral n)
  in bixixf bix ix

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
  $ \wf -> ForAll bs
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
  $ \wf -> ForAll bn
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
mapPermSeqG f inp outp pull@(GlobPull bs ixf) =

  GlobPush (bn * fromIntegral outN)
  $ \wf -> ForAllBlocks
           $ \bix -> ForAll bn
                     $ \tix ->
                     let p = gatherSeq pull
                         dat = f (p ! (bix * fromIntegral bn + tix))  -- TODO: maybe should be bs and not bn.
                     in sequence_ [wf (dat !! i) ((outp (bix * fromIntegral bn + tix)) !! i) -- TODO: same as above.
                                  | i <- [0..outN-1]]
  where 
    bn = bs `div` fromIntegral inN 
    gatherSeq (GlobPull n ixf) =
      GlobPull (n `div` fromIntegral inN)
      $ \ix -> [ixf i | i <- inp ix]
               
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
getTest3 = quickPrint (forceG2 . blockView . test3 . changeIn )
                       (sizedGlobal2 256)

getTest3' = quickPrint (forceG . test3 )
                       (sizedGlobal 256)



-- TODO: Probably lots of bugs right now


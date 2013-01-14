{-# LANGUAGE ScopedTypeVariables,
             FlexibleContexts #-} 

module Examples where

import qualified Foreign.CUDA.Driver as CUDA
import qualified Foreign.CUDA.Driver.Device as CUDA
import qualified Obsidian.CodeGen.CUDA as CUDA
import Obsidian.Run.CUDA.Exec

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
scalArg :: EInt -> Distrib (Pull EInt) -> Final (GProgram (Distrib (Pull EInt))) -- GlobArray EInt
--scalArg :: EInt -> Distrib (Pull EInt) -> Distrib (BProgram (Pull EInt))
scalArg e = forceG . toGlobPush. fmap (force . fmap (+e))

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

input2 :: Distrib (Pull EInt)
input2 = namedGlobal "apa"  32

input3 :: Distrib (Pull (Exp Int32))
input3 = namedGlobal "apa"  32
 
---------------------------------------------------------------------------
-- Small experiments 
---------------------------------------------------------------------------

sync :: Forceable a => a -> BProgram (Forced a)
sync = force 

prg0 = putStrLn$ printPrg$  mapFusion input1

mapFusion' :: Distrib (Pull EInt)
              -> Distrib (BProgram (Pull EInt))
mapFusion' arr = mapD mapFusion arr
                 
prg1 = putStrLn$ printPrg$ cheat $ (forceG . toGlobPush . mapFusion') input2


---------------------------------------------------------------------------
-- Permutation test
--------------------------------------------------------------------------- 
--  a post permutation (very little can be done with a GlobArray) 
permuteGlobal :: (Exp Word32 -> Exp Word32 -> (Exp Word32, Exp Word32))
                 -> Distrib (Pull a)
                 -> GlobPush a
permuteGlobal perm distr{-@(Distrib nb bixf)-} = 
  GlobPush bs $
    \wf -> -- (a -> W32 -> W32 -> TProgram)
       do
         ForAllBlocks $
           \bix -> ForAll bs $
                   \tix ->
                   let (bix',tix') = perm bix tix 
                   in wf ((getBlock distr bix) ! tix) bix' tix'
  where
--    nb = numBlocks distr 
    bs = len (getBlock distr 0) -- bixf 0)

--Complicated. 
permuteGlobal' :: (Exp Word32 -> Exp Word32 -> (Exp Word32, Exp Word32))
                 -> Distrib (BProgram (Pull a))
                 -> GlobPush a
permuteGlobal' perm distr@(Distrib bixf) = 
  GlobPush bs $
    \wf -> -- (a -> W32 -> W32 -> TProgram)
       do
         ForAllBlocks $
           \bix ->
           do -- BProgram do block
             arr <- bixf bix
             ForAll bs $ 
               \tix ->
                 let (bix',tix') = perm bix tix
                 in wf (arr ! tix) bix' tix'
  where
    -- Gah. (Does this even work? (for real?)) 
    bs = len $ fst $ runPrg 0 $ bixf 0

---------------------------------------------------------------------------
-- mapD experiments
---------------------------------------------------------------------------
mapD :: (a -> BProgram b) ->
        (Distrib a -> Distrib (BProgram b))
mapD f inp@(Distrib bixf) =
  Distrib $ \bid -> f (bixf bid)



---------------------------------------------------------------------------
-- Playing with CUDA launch code generation.
-- Much work needed here.
---------------------------------------------------------------------------

{-
test = putStrLn $ getCUDA $
         do
           kernel <- cudaCapture (forceBT . toGlobPush . mapFusion') input2

           i1 <- cudaUseVector (V.fromList [0..31 :: Int32]) Int32
           o1 <- cudaAlloca 32 Int32
         
           cudaTime "Timing execution of kernel" $ 
             cudaExecute kernel 1 32 [i1] [o1] 

           cudaFree i1
           cudaFree o1 
             
           return ()
-} 

test1 = withCUDA $
         do
           kernel <- capture (forceG . toGlobPush . mapFusion') input2

           useVector (V.fromList [0..31 :: Int32]) $ \ i1 ->
              allocaVector 32 $ \(o1 :: CUDA.DevicePtr Int32) ->
              --cudaTime "Timing execution of kernel" $
                do
                  -- TODO: Get sharedmem size from some analysis
                  execute kernel 1 512 i1 o1
                  r <- lift $ CUDA.peekListArray 32 o1
                  lift $ putStrLn $ show r 

           
             
         
---------------------------------------------------------------------------
--
-- Countingsort start
--
---------------------------------------------------------------------------

gatherGlobal :: Distrib (Pull (Exp Word32)) 
                -> Exp Word32 -- expected output size number of blocks
                -> Word32     -- expected output size block-size
                -> Distrib (Pull a)
                -> GlobPush a
gatherGlobal indices@(Distrib inf)
             nb bs
             elems@(Distrib enf) =
  GlobPush bs $
   \wf ->
     ForAllBlocks $ \ bid -> 
       ForAll bs $ \ tid -> 
         let  inArr = inf bid
              inix  = inArr ! tid

              bid'  = inix `div` fromIntegral bs
              tid'  = inix `mod` fromIntegral bs  
              e     = (enf bid') ! tid' 
         in wf e bid tid

scatterGlobal :: Distrib  (Pull (Exp Word32)) -- where to scatter
                 -- ->  Exp Word32 -- output size
                 ->  Word32     -- block size
                 -> Distrib (Pull a) -- the elements to scatter
                 -> GlobPush a 
scatterGlobal indices bs elems = 
  GlobPush bs $
    \wf -> ForAllBlocks $ \bid ->
    ForAll bs $ \tid ->
    let  inArr = getBlock indices bid
         inix  = inArr ! tid
         bid'  = inix `div` fromIntegral bs
         tid'  = inix `mod` fromIntegral bs 
         e     = (getBlock elems bid) ! tid 
    in wf e bid' tid' 
        
distribute :: Word32 -> a -> Distrib (Pull a)
distribute bs e = Distrib $ \bid -> replicate bs e           

-- DONE: Error. gather is not the operation you want here!
--   changed to Scatter. (see if concepts are right) 
histogram :: -- Exp Word32
              Word32
             -> Distrib (Pull (Exp Word32))
             -> GlobPush (Exp Word32)
histogram bs elems = scatterGlobal elems bs (distribute bs 1)
  -- where nb = numBlocks elems

reconstruct :: Distrib (Pull (Exp Word32))
               -> Distrib (Pull (Exp Word32))
               -> GlobPush (Exp Word32)
reconstruct inp{-@(Distrib nb bixf)-} pos{-@(Distrib _ posf)-} =
  permuteGlobal perm inp 
  where
    perm bix tix =
      let bs  = len (getBlock inp bix) -- (bixf bix) 
          gix = (getBlock inp bix) ! tix
          bix' = gix `div` (fromIntegral bs)
          tix' = gix `mod` (fromIntegral bs)

          pgix = (getBlock pos bix') ! tix'
          pbix = pgix `div` (fromIntegral bs)
          ptix = pgix `mod` (fromIntegral bs) 
      in (pbix,ptix)


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

{- 
fuseMaximi :: Distrib (Pull (Exp Word32))
              -> Distrib (Pull (Exp Word32))
              -> GlobPush (Exp Word32) -- Distrib (BProgram (Pull (Exp Word32)))
-- make this prettier
fuseMaximi a b = toGlobPush $ 
  Distrib (numBlocks b) $
  \bix -> force (zipWith (+) (getBlock a bix)
                             (getBlock b bix))

-- gets a sync that it does not (really) need. 
maxDist :: Distrib (Pull (Exp Word32)) -> GlobPush (Exp Word32)
maxDist inp = toGlobArray $ fmap force (replBlockMaximi inp)
   -}           


---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------

test2 = withCUDA $
         do
           hist   <- capture (forceG . (histogram 255))
                             (sizedGlobal  256 :: DistArray (Exp Word32))
           --kernel <- capture (forceG . toGlobArray . mapFusion') input2

           useVector (V.fromList (P.replicate 256 (7::Word32)) {-[0..255 :: Int32]-} ) $ \ i1 ->
              useVector (V.fromList (P.replicate 256 0)) $ \(o1 :: CUDA.DevicePtr Int32) ->
              --cudaTime "Timing execution of kernel" $
                do
                  -- TODO: Get sharedmem size from some analysis
                  execute hist 1 256 i1 o1
                  r <- lift $ CUDA.peekListArray 256 o1
                  lift $ putStrLn $ show r 




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
               let pully = Pull n (\ix -> ixf (bix * (fromIntegral n) + ix))
               res <- f pully
               ForAll n $ \ix -> wf (res ! ix) bix ix

mapG2 :: (Pull a -> BProgram (Pull b))
         -> GlobPull2 a
         -> GlobPush b
mapG2 f (GlobPull2 n bixixf) =
  GlobPush n
  $ \wf -> ForAllBlocks
           $ \bix ->
           do -- BProgram do block
             let pully = Pull n (\ix -> bixixf bix ix)
             res <- f pully
             ForAll n $ \ix -> wf (res ! ix) bix ix 

mapG' :: (Pull a -> BProgram (Pull b))
         -> GlobPull a
         -> Distrib (BProgram (Pull b))
mapG' f (GlobPull n ixf) =
  Distrib 
          $ \bix ->
            let pully = Pull n (\ix -> ixf (bix * (fromIntegral n) + ix))
            in  f pully


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

-- Remove this after removing all occurances of Distrib 
silly :: Distrib (Pull a) -> GlobPull2 a
silly (Distrib bixf) = GlobPull2 n $ \bix ix -> (bixf bix) ! ix
  where
    n = len (bixf 0) 


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
              -> (Exp Word32 -> [Exp Word32])
              -> (Exp Word32 -> [Exp Word32]) -> Pull a -> Push b
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
               -> (Exp Word32 -> [Exp Word32])
               -> (Exp Word32 -> [Exp Word32])  -> GlobPull a -> GlobPush' b
mapPermSeqG f inp outp pull@(GlobPull bs ixf) =

  GlobPush' (bn * fromIntegral outN)
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
--
---------------------------------------------------------------------------

test3 :: GlobPull (Exp Int32) -> GlobPush' (Exp Int32)
test3 = mapPermSeqG (\[a,b] -> [min a b, max a b])
                    (\ix -> [ix, ix + 1024])
                    (\ix -> [ix, ix + 1024])



  
--test2' :: GlobPull (Exp Int32)
--         -> GlobPush (Exp Int32)
--test2' = mapG (force . testBy)

getTest3 = quickPrint (forceG . conv2 . test3 . changeIn . silly)
                       (sizedGlobal 256)


-- TODO: Probably lots of bugs right now
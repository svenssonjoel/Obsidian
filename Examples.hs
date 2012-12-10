{-# LANGUAGE FlexibleInstances,
             FlexibleContexts, 
             ScopedTypeVariables,
             RankNTypes  #-} 
module Examples where 


import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA
import Obsidian.GCDObsidian.CodeGen.CUDA.WithCUDA
import Obsidian.GCDObsidian.CodeGen.CUDA.Compile
import qualified Foreign.CUDA.Driver as CUDA

import qualified Obsidian.GCDObsidian.CodeGen.Program as CGP
import           Obsidian.GCDObsidian.CodeGen.InOut

import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Library
import Obsidian.GCDObsidian.Force


import Data.Word
import Data.Int
import Data.Bits

import qualified Data.Vector.Storable as V

import Control.Monad.State

import Prelude hiding (zipWith,sum,replicate)
import qualified Prelude as P 

---------------------------------------------------------------------------
-- MapFusion example
---------------------------------------------------------------------------

mapFusion :: Array Pull EInt -> BProgram (Array Pull EInt)
mapFusion arr =
  do
    imm <- sync $ (fmap (+1) . fmap (*2)) arr
    sync $ (fmap (+3) . fmap (*4)) imm 

input1 :: Array Pull EInt 
input1 = namedArray "apa" 32

input2 :: Distrib (Array Pull EInt)
input2 = namedGlobal "apa" 256 32

sync :: forall a. Scalar a
        => Array Pull (Exp a) -> BProgram (Array Pull (Exp a))
sync (Array n (Pull ixf)) =
  do
    name <- BAllocate (n*fromIntegral (sizeOf (undefined :: Exp a)))
                      (Pointer (typeOf (undefined :: Exp a)))

    p (targetArr name) 
    BSync
    return $ Array n $ Pull (\i -> index name i) 
      
    where
      p = \k -> BForAll n $ (\ix -> k (ixf ix) ix)  
      targetArr name e i = TAssign name i e

prg0 = putStrLn$ printPrg$ toProg $ mapFusion input1

mapFusion' :: Distrib (Array Pull EInt)
              -> Distrib (BProgram (Array Pull EInt))
mapFusion' arr = mapD mapFusion arr

toGlobArray :: forall a. Scalar a
               => Distrib (BProgram (Array Pull (Exp a)))
               -> GlobArray2 (Exp a)
toGlobArray inp@(Distrib nb bixf) =
  GlobArray2 nb bs $
    \wf -> GForAll nb $
           \bix ->
           do -- BProgram do block 
             arr <- bixf bix 
             BForAll bs $ \ix -> wf (arr ! ix) bix ix 
  where
    bs = len $ fst $ runPrg 0 $ toProg (bixf 0) 
  

forceBT :: forall a. Scalar a => GlobArray2 (Exp a)
           -> Final (GProgram (Distrib (Array Pull (Exp a))))
forceBT (GlobArray2 nb bs pbt) = Final $ 
  do
      global <- GOutput $ Pointer (typeOf (undefined :: Exp a))
      
      pbt (assignTo global bs)
        
      return $ Distrib nb  $ 
        \bix -> Array bs (Pull (\ix -> index global ((bix * (fromIntegral bs)) + ix)))
    where 
      assignTo name s e b i = TAssign name ((b*(fromIntegral s))+i) e


prg1 = putStrLn$ printPrg$ toProg $ cheat $ (forceBT . toGlobArray . mapFusion') input2


---------------------------------------------------------------------------
-- Permutation test
--------------------------------------------------------------------------- 
--  a post permutation (very little can be done with a GlobArray) 
permuteGlobal :: (Exp Word32 -> Exp Word32 -> (Exp Word32, Exp Word32))
                 -> Distrib (Array Pull a)
                 -> GlobArray2 a
permuteGlobal perm distr@(Distrib nb bixf) = 
  GlobArray2 nb bs $
    \wf -> -- (a -> W32 -> W32 -> TProgram)
       do
         GForAll nb $
           \bix -> BForAll bs $
                   \tix ->
                   let (bix',tix') = perm bix tix 
                   in wf ((bixf bix) ! tix) bix' tix'
  where 
    bs = len (bixf 0)

--Complicated. 
permuteGlobal' :: (Exp Word32 -> Exp Word32 -> (Exp Word32, Exp Word32))
                 -> Distrib (BProgram (Array Pull a))
                 -> GlobArray2 a
permuteGlobal' perm distr@(Distrib nb bixf) = 
  GlobArray2 nb bs $
    \wf -> -- (a -> W32 -> W32 -> TProgram)
       do
         GForAll nb $
           \bix ->
           do -- BProgram do block
             arr <- bixf bix
             BForAll bs $ 
               \tix ->
                 let (bix',tix') = perm bix tix 
                 in wf (arr ! tix) bix' tix'
  where
    -- Gah. (Does this even work? (for real?)) 
    bs = len $ fst $ runPrg 0 $ toProg  $ bixf 0




{- 
---------------------------------------------------------------------------
-- Sync, Force. What to use? what to scrap ? 
---------------------------------------------------------------------------
-- very limiting type.. 
sync :: Scalar a => Array Pull (Exp a) -> Program (Array Pull (Exp a))
sync = force

             
---------------------------------------------------------------------------
-- Print Programs for test
---------------------------------------------------------------------------
prg0 = putStrLn$ printPrg$ mapFusion input1
--prg1 = putStrLn$ printPrg$ testG1 inputG


---------------------------------------------------------------------------
-- Translate and pring as CGP.Programs 
---------------------------------------------------------------------------
prg0' = putStrLn$ CGP.printPrg$ CGP.runPrg (mapFusion input1)
--prg1' = putStrLn$ CGP.printPrg$ CGP.runPrg (testG1 inputG)


---------------------------------------------------------------------------
-- Counting sort experiments
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Histogram
---------------------------------------------------------------------------
{- 
histogram :: Exp Word32
             -> Blocks (Array Pull (Exp Word32))
             -> Blocks (Array Push (Exp Word32))
histogram maxLen (Blocks nb blkf)  =
  Blocks nb blkf' 
  where                
    blkf' bid = Array blkSize (Push (collect bid)) 
    blkSize = len (blkf 0) -- all blocks are same size  
    collect bid k = ForAll blkSize $ \i ->
      k ((blkf bid) ! i,1)
-} 
{- 
histogram :: Blocks (Array Pull (Exp Word32))
             -> GlobArray PushBT (Exp Word32)
histogram (Blocks nb blkf) =
  GlobArray nb 256 $ PushP (\wf bix ->
              let arr = blkf bix
                  blkSize = len arr
              in ForAll blkSize $ \i ->
                  let ix' = arr ! i
                      blk = ix' `div` fromIntegral blkSize
                      ix  = ix' `mod` fromIntegral blkSize  
                  in  wf ix 1 blk)
-}

{- 
histogram :: Blocks (Array Pull (Exp Word32))
             -> GlobArray PushBT (Exp Word32)
histogram (Blocks nb blkf) =
  GlobArray nb blkSize $ PushP (\wf bix tix ->
              let arr = blkf bix
                  ix' = arr ! tix
                  blk = ix' `div` fromIntegral blkSize
                  ix  = ix' `mod` fromIntegral blkSize  
              in  wf 1 blk ix)
    where
      blkSize = len (blkf 0)

-}
{-
 

-}
{-
forceBT :: forall a. Scalar a => GlobArray PushBT (Exp a) -> Program (Blocks (Array Pull (Exp a)))
forceBT (GlobArray nb bs (PushP pbt)) =
  do
      global <- Output $ Pointer (typeOf (undefined :: (Exp a)))
      
      ForAllBlocks nb 
        (\bid ->
          ForAll bs $ \ix -> 
            (pbt (assignTo global bs)) bid ix)
        
      return $ Blocks nb  $ 
        \bix -> Array bs (Pull (\ix -> index global ((bix * (fromIntegral bs)) + ix)))
    where 
      assignTo name s e b i = Assign name ((b*(fromIntegral s))+i) e
      
hist
  :: Exp Word32
     -> Blocks (Array Pull (Exp Word32))
     -> Program (Blocks (Array Pull (Exp Word32)))
hist max inp = forceBT (histogram {-max-} inp)

inputWord32 :: Blocks (Array Pull (Exp Word32)) 
inputWord32 = namedGlobal "apa" (variable "N") 256


getHist = putStrLn$ CUDA.genKernel "hist" (hist 256)  inputWord32

-} 
---------------------------------------------------------------------------
-- Scan  (TODO: Rewrite as a exclusive scan (0 as first elem in result) 
---------------------------------------------------------------------------
sklanskyLocal
  :: (Num (Exp a), Scalar a) =>
     Int
     -> (Exp a -> Exp a -> Exp a)
     -> Array Pull (Exp a)
     -> Program (Array Pull (Exp a))
sklanskyLocal 0 op arr = return (shiftRight 1 0 arr)
sklanskyLocal n op arr =
  do 
    let arr1 = twoK (n-1) (fan op) arr
    arr2 <- sync arr1
    sklanskyLocal (n-1) op arr2
                     

fan op arr =  a1 `conc`  fmap (op c) a2 
    where 
      (a1,a2) = halve arr
      c = a1 ! (fromIntegral (len a1 - 1))


sklanskyAllBlocks :: Int
                     -> Distrib (Array Pull (Exp Int32))
                     -> Distrib (Program (Array Pull (Exp Int32)))
sklanskyAllBlocks logbsize arr =
  mapD (sklanskyLocal logbsize (+)) arr
   
---------------------------------------------------------------------------
-- Testing WithCUDA aspects
---------------------------------------------------------------------------

testGRev :: Distrib (Array Pull EWord32)
            -> Distrib (Program (Array Pull EWord32))
testGRev = mapD (force . push . rev)

---------------------------------------------------------------------------
--
--  NEW PART
--
---------------------------------------------------------------------------

{-
   I want to think of mapD like this:

    f is a program on local data that potentially stores results
    in local memory (thats what the Program part in the return type means).

    mapD f takes an array distributed over a number of "blocks"
    (Not physically distributed at this time). The result on the
    other hand is potentially stored distributedly(wow thats really a word!)
    over the shared memories (thats the Program part inside of the Distrib).

   Questions: 
    Im not sure that it is impossible to missuse mapD to somehow affect
    elements outside of the "local" part of the array. How do I make sure so ?

   Thoughts: It should be possible to compose mapD's .. mapD f . mapD g.
    since there is no communication across blocks.

    Anything that actually communicates values across block boundaries
    should have the GlobArray return type instead of Distrib Something.
    (It should be impossible to go from a GlobArray  to a Distrib something
    inside a kernel) 
    
   

-} 
-}
class LocalArrays a
instance LocalArrays (Array Pull a) 
instance LocalArrays (Array Push a)
instance LocalArrays (Array BPush a)
instance (LocalArrays a, LocalArrays b) => LocalArrays (a,b)
instance (LocalArrays a, LocalArrays b, LocalArrays c) => LocalArrays (a,b,c)
  

mapD :: (LocalArrays a, LocalArrays b) =>
        (a -> BProgram b) ->
        (Distrib a -> Distrib (BProgram b))
mapD f inp@(Distrib nb bixf) =
  Distrib nb $ \bid -> f (bixf bid)
{- 

-- Incorrect.
-- I suspect turning into a GlobArray really requires
-- the data to be copied. (and the programs "sequenced")

toGlobArray :: Distrib (Program (Array Pull a)) -> GlobArray a
toGlobArray inp@(Distrib nb bixf) =
  GlobArray nb bs $     
    \wf bid tid ->
      do
        arr <- bixf bid 
        wf (arr ! tid) bid tid
  where
    -- Maybe not very efficient.. 
    bs = len $ fst $ runPrg 0 (bixf 0)


--  a post permutation (very little can be done with a GlobArray) 
permuteGlobal :: (Exp Word32 -> Exp Word32 -> (Exp Word32, Exp Word32))
                 -> Distrib (Array Pull a)
                 -> GlobArray a
permuteGlobal perm distr@(Distrib nb bixf) = 
  GlobArray nb bs $
    \wf bid tid ->
      do
        -- TODO: I Think this is wrong. 
        let (bid',tid') = perm bid tid
        -- I changed order here.. (bid tid bid' tid') 
        wf ((bixf bid') ! tid') bid tid 
 where 
  bs = len (bixf 0)

--  a post permutation (very little can be done with a GlobArray) 
permuteGlobal' :: (Exp Word32 -> Exp Word32 -> (Exp Word32, Exp Word32))
                 -> Distrib (Program (Array Pull a))
                 -> Program (GlobArray a)
permuteGlobal' perm distr@(Distrib nb bixf) =
  do
    inner <- bixf 0
    let bs = len inner
    return $ GlobArray nb bs $
      \wf bid tid ->
      do
        arr <- bixf bid
      
        -- TODO: I Think this is wrong. 
        let (bid',tid') = perm bid tid
        -- I changed order here.. (bid tid bid' tid') 
        wf (arr ! tid') bid tid 

-- The code generator needs to insert suitable conditionals
-- if the number of blocks to output are fewer than those we read from 
gatherGlobal :: Distrib (Array Pull (Exp Word32))
                -> Exp Word32 -- expected output size number of blocks
                -> Word32     -- expected output size block-size
                -> Distrib (Array Pull a)
                -> GlobArray a
gatherGlobal indices@(Distrib nbs inf)
             nb bs
             elems@(Distrib ebs enf) =
  GlobArray nb bs $
   \wf bid tid ->
     let  inArr = inf bid
          inix  = inArr ! tid

          bid'  = (inix `div` (fromIntegral bs))
          tid'  = (inix `mod` (fromIntegral bs))  
          e     = (enf bid) ! tid 
     in wf e bid tid
           
distribute :: Exp Word32 -> Word32 -> a -> Distrib (Array Pull a)
distribute nb bs e = Distrib nb $ \bid -> replicate bs e  

histogram :: Num a
             => Exp Word32
             -> Word32
             -> Distrib (Array Pull (Exp Word32))
             -> GlobArray a
histogram nb bs elems = gatherGlobal elems nb bs (distribute nb bs 1)


-- Im not sure this one is right. (Try to get to code generation soon) 
reconstruct inp@(Distrib nb bixf) pos@(Distrib _ posf) =
  permuteGlobal perm inp 
  where
    perm bix tix =
      let bs  = len (bixf bix) 
          gix = (bixf bix) ! tix
          bix' = gix `div` (fromIntegral bs)
          tix' = gix `mod` (fromIntegral bs)

          pgix = (posf bix') ! tix'
          pbix = pgix `div` (fromIntegral bs)
          ptix = pgix `mod` (fromIntegral bs) 
      in (pbix,ptix)


---------------------------------------------------------------------------
-- force a GlobArray
---------------------------------------------------------------------------
forceBT :: forall a. Scalar a => GlobArray (Exp a)
           -> Final (Program (Distrib (Array Pull (Exp a))))
forceBT (GlobArray nb bs pbt) = Final $ 
  do
      global <- Output $ Pointer (typeOf (undefined :: Exp a))
      
      ForAllBlocks nb 
        (\bid ->
          ForAll bs $ \ix -> 
            (pbt (assignTo global bs)) bid ix)
        
      return $ Distrib nb  $ 
        \bix -> Array bs (Pull (\ix -> index global ((bix * (fromIntegral bs)) + ix)))
    where 
      assignTo name s e b i = Assign name ((b*(fromIntegral s))+i) e


---------------------------------------------------------------------------
-- Experiment: reverse a GlobArray
---------------------------------------------------------------------------
reverseGA :: GlobArray (Exp Int32) -> GlobArray (Exp Int32) 
reverseGA (GlobArray nb bs pbt) =
  GlobArray nb bs $ \wf bid tid ->
      pbt wf (nb - 1 - bid)
             (fromIntegral bs - 1 - tid)
             



---------------------------------------------------------------------------
-- Get Launcher as text experiment. (For the windows users!) 
---------------------------------------------------------------------------
launcher1 =
  do
    myFun <- cudaCapture (forceBT . toGlobArray . sklanskyAllBlocks 3)
                         (sizedGlobal (variable "N") 256)

    d1 <- cudaUseVector (V.fromList [0..255::Int32]) Int32    
    d2 <- cudaAlloca 256 Int32

    cudaTime "Timing kernel execution: "
      $ do 
        cudaExecute myFun 1 2048 [d1] [d2] 
    
    return 10





---------------------------------------------------------------------------
-- Local Push array experiments
---------------------------------------------------------------------------

revP :: Pushable (Array p) => Array p a -> Array Push a
revP arr = Array n $Push $ \wf -> p (\(ix,a) -> wf (fromIntegral n - 1 - ix,a))
  where
    parr@(Array n (Push p)) = push arr

revP2 :: Array (PushP (Exp Word32 -> Program ())) a ->
         Array (PushP (Exp Word32 -> Program ())) a
revP2 (Array n (PushP p)) =
  Array n (PushP
           (\wf tid ->
             let wf' a ix = wf a (fromIntegral n - 1 - ix)  
             in p wf' tid))

push' (Array n (Pull ixf)) =
  Array n $
  PushP $ \wf tid -> wf (ixf tid) tid

revP2Test = printPrg (ForAll n $ \ix -> prg ix) 
  where prg = pfun (write n)
        (Array n (PushP pfun)) = revP2 (push' arr)
        write n a ix =
          Assign "bepa" ix a 
        arr = namedArray "apa" 256 :: Array Pull (Exp Int32)


twoKP :: Int -> (Array Pull a -> Array (PushP (Exp Word32 -> Program ()))  b)
         -> Array Pull a -> Array (PushP (Exp Word32 -> Program ())) b
twoKP k f arr = undefined 
  where
    templ = len arr `div` (2^k) 
    arr'  = resize templ arr 

    
    parr  = f arr' 
    {-
      i parr finns kärnan av en program som beskriver hur man utför
      f på en liten del av input arrayen.
      Jag funderar på om det finns ngt sätt att manipulera programmet
      för att beskriva f beräkningen replicerad över hela input arrayen.
    -}


---------------------------------------------------------------------------
-- Get somewhere on the codegen front 
---------------------------------------------------------------------------
testPermute :: Distrib (Array Pull (Exp Int32)) -> GlobArray (Exp Int32)
testPermute input@(Distrib nb disf) = permuteGlobal perm input 
  where
    bs = len $ disf 0
    perm bid tid = (nb - 1 - bid,
                    fromIntegral bs - 1 - tid) -- reverse in other words.

-- This shows that codegen needs to take care of the "number of blocks", N0
-- in this case. 
printTestPermute = putStrLn 
                   $ CUDA.genKernel "perm"
                   (forceBT . testPermute) (sizedGlobal undefined 256)


-- Testing Testing.
testScanThenReverse :: Distrib (Array Pull (Exp Int32)) -> Program (GlobArray (Exp Int32))
testScanThenReverse input@(Distrib nb disf) =
  do
    let bs = len (disf 0)
    permuteGlobal' perm res
   where
     res = sklanskyAllBlocks 3 input
     bs = len $ disf 0
     perm bid tid = (nb - 1 - bid,
                     fromIntegral bs - 1 - tid) -- reverse in other words.

-- Turn a (Program (GlobArray a)) into a GlobArray a. Possible ?
--f :: Program (GlobArray a) -> GlobArray a
--f prg = GlobArray nb bs $ \ a bid tid -> undefined
-- I think this is problematic.
--  But does it indicate some larger issues about the
--  mapD and Distrib setup in general ?
--  is there a way to get this functionality without these problems?

-} 

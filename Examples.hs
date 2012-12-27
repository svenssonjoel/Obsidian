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

import Data.Word
import Data.Int
import Data.Bits

import qualified Data.Vector.Storable as V

import Control.Monad.State

import Prelude hiding (zipWith,sum,replicate)
import qualified Prelude as P 

{-
   -- TODO: SeqFor finns i Program. Fixa codegen.
   -- Force: bry inte om generalisera nu (eller ngnsin). 
   -- Countingsort: generera kod, se att funkar.
   -- Riktig Countingsort: TODO!
-} 

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
input2 = namedGlobal "apa" 256 32

input3 :: Distrib (Pull (Exp Int32))
input3 = namedGlobal "apa" 256 32
 
---------------------------------------------------------------------------
--
---------------------------------------------------------------------------

sync :: Forceable a => a -> BProgram (Forced a)
sync = force 

prg0 = putStrLn$ printPrg$  mapFusion input1

mapFusion' :: Distrib (Pull EInt)
              -> Distrib (BProgram (Pull EInt))
mapFusion' arr = mapD mapFusion arr


toGlobArray :: Distrib (BProgram (Pull a))
               -> GlobArray a               
toGlobArray inp@(Distrib nb bixf) =
  GlobArray nb bs $
    \wf -> ForAllBlocks nb $
           \bix ->
           do -- BProgram do block 
             arr <- bixf bix 
             ForAll bs $ \ix -> wf (arr ! ix) bix ix 
  where
    bs = len $ fst $ runPrg 0 $ bixf 0
  

forceBT :: forall a. Scalar a => GlobArray (Exp a)
           -> Final (GProgram (Distrib (Pull (Exp a))))
forceBT (GlobArray nb bs pbt) = Final $ 
  do
      global <- Output $ Pointer (typeOf (undefined :: Exp a))
      
      pbt (assignTo global bs)
        
      return $ Distrib nb  $ 
        \bix -> (Pull bs (\ix -> index global ((bix * (fromIntegral bs)) + ix)))
    where 
      assignTo name s e b i = Assign name ((b*(fromIntegral s))+i) e


prg1 = putStrLn$ printPrg$ cheat $ (forceBT . toGlobArray . mapFusion') input2


---------------------------------------------------------------------------
-- Permutation test
--------------------------------------------------------------------------- 
--  a post permutation (very little can be done with a GlobArray) 
permuteGlobal :: (Exp Word32 -> Exp Word32 -> (Exp Word32, Exp Word32))
                 -> Distrib (Pull a)
                 -> GlobArray a
permuteGlobal perm distr@(Distrib nb bixf) = 
  GlobArray nb bs $
    \wf -> -- (a -> W32 -> W32 -> TProgram)
       do
         ForAllBlocks nb $
           \bix -> ForAll bs $
                   \tix ->
                   let (bix',tix') = perm bix tix 
                   in wf ((bixf bix) ! tix) bix' tix'
  where 
    bs = len (bixf 0)

--Complicated. 
permuteGlobal' :: (Exp Word32 -> Exp Word32 -> (Exp Word32, Exp Word32))
                 -> Distrib (BProgram (Pull a))
                 -> GlobArray a
permuteGlobal' perm distr@(Distrib nb bixf) = 
  GlobArray nb bs $
    \wf -> -- (a -> W32 -> W32 -> TProgram)
       do
         ForAllBlocks nb $
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
class LocalArrays a
instance LocalArrays (Pull a) 
instance LocalArrays (Push a)
instance (LocalArrays a, LocalArrays b) => LocalArrays (a,b)
instance (LocalArrays a, LocalArrays b, LocalArrays c) => LocalArrays (a,b,c)
  

mapD :: (LocalArrays a, LocalArrays b) =>
        (a -> BProgram b) ->
        (Distrib a -> Distrib (BProgram b))
mapD f inp@(Distrib nb bixf) =
  Distrib nb $ \bid -> f (bixf bid)



---------------------------------------------------------------------------
-- Playing with CUDA launch code generation.
-- Much work needed here.
---------------------------------------------------------------------------

{-
test = putStrLn $ getCUDA $
         do
           kernel <- cudaCapture (forceBT . toGlobArray . mapFusion') input2

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
           kernel <- capture (forceBT . toGlobArray . mapFusion') input2

           useVector (V.fromList [0..31 :: Int32]) $ \ i1 ->
              allocaVector 32 $ \(o1 :: CUDA.DevicePtr Int32) ->
              --cudaTime "Timing execution of kernel" $
                do
                  -- TODO: Get sharedmem size from some analysis
                  execute kernel 1 512 i1 o1
                  r <- lift $ CUDA.peekListArray 32 o1
                  lift $ putStrLn $ show r 

           
             
           


---------------------------------------------------------------------------
-- Strange push array 
---------------------------------------------------------------------------

push1 = push $ zipp (input1,input1)

testApa =  printPrg $ write_ push1




---------------------------------------------------------------------------
--
-- Countingsort start
--
---------------------------------------------------------------------------

gatherGlobal :: Distrib (Pull (Exp Word32))
                -> Exp Word32 -- expected output size number of blocks
                -> Word32     -- expected output size block-size
                -> Distrib (Pull a)
                -> GlobArray a
gatherGlobal indices@(Distrib nbs inf)
             nb bs
             elems@(Distrib ebs enf) =
  GlobArray nb bs $
   \wf ->
     ForAllBlocks nb $ \ bid ->
     do
       ForAll bs $ \ tid -> 
         let  inArr = inf bid
              inix  = inArr ! tid

              bid'  = (inix `div` (fromIntegral bs))
              tid'  = (inix `mod` (fromIntegral bs))  
              e     = (enf bid) ! tid 
         in wf e bid tid
        
distribute :: Exp Word32 -> Word32 -> a -> Distrib (Pull a)
distribute nb bs e = Distrib nb $ \bid -> replicate bs e           

histogram :: Num a
             => Exp Word32
             -> Word32
             -> Distrib (Pull (Exp Word32))
             -> GlobArray a
histogram nb bs elems = gatherGlobal elems nb bs (distribute nb bs 1)

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
                     

fan op arr =  a1 `conc`  fmap (op c) a2 
    where 
      (a1,a2) = halve arr
      c = a1 ! (fromIntegral (len a1 - 1))


sklanskyAllBlocks :: Int
                     -> Distrib (Pull (Exp Int32))
                     -> Distrib (BProgram (Pull (Exp Int32)))
sklanskyAllBlocks logbsize arr =
  mapD (sklanskyLocal logbsize (+)) arr



printSklansky = putStrLn
                $ CUDA.genKernel "sklansky"
                  (cheat . forceBT . toGlobArray . sklanskyAllBlocks 3) input3 

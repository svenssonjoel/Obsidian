{-# LANGUAGE FlexibleInstances,
             FlexibleContexts, 
             ScopedTypeVariables,
             RankNTypes  #-} 
module Examples where 


import qualified Obsidian.CodeGen.CUDA as CUDA
import Obsidian.CodeGen.CUDA.WithCUDA
import Obsidian.CodeGen.CUDA.Compile
import qualified Foreign.CUDA.Driver as CUDA

import qualified Obsidian.CodeGen.Program as CGP
import           Obsidian.CodeGen.InOut

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

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------

sync = force 

prg0 = putStrLn$ printPrg$ toProg $ mapFusion input1

mapFusion' :: Distrib (Pull EInt)
              -> Distrib (BProgram (Pull EInt))
mapFusion' arr = mapD mapFusion arr

toGlobArray :: forall a. Scalar a
               => Distrib (BProgram (Pull (Exp a)))
               -> GlobArray (Exp a)
toGlobArray inp@(Distrib nb bixf) =
  GlobArray nb bs $
    \wf -> GForAll nb $
           \bix ->
           do -- BProgram do block 
             arr <- bixf bix 
             BForAll bs $ \ix -> wf (arr ! ix) bix ix 
  where
    bs = len $ fst $ runPrg 0 $ toProg (bixf 0) 
  

forceBT :: forall a. Scalar a => GlobArray (Exp a)
           -> Final (GProgram (Distrib (Pull (Exp a))))
forceBT (GlobArray nb bs pbt) = Final $ 
  do
      global <- GOutput $ Pointer (typeOf (undefined :: Exp a))
      
      pbt (assignTo global bs)
        
      return $ Distrib nb  $ 
        \bix -> (Pull bs (\ix -> index global ((bix * (fromIntegral bs)) + ix)))
    where 
      assignTo name s e b i = TAssign name ((b*(fromIntegral s))+i) e


prg1 = putStrLn$ printPrg$ toProg $ cheat $ (forceBT . toGlobArray . mapFusion') input2


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
         GForAll nb $
           \bix -> BForAll bs $
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

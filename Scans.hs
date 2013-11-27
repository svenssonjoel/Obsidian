{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-} 
module Scans where 

import Obsidian

import Prelude hiding (zipWith,replicate)
import qualified Prelude as P 
import Obsidian
import Obsidian.Run.CUDA.Exec
-- import Obsidian.Run.CUDA.SC

import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Int
import Control.Monad

import Obsidian.CodeGen.Program 

reduceLocal :: MemoryOps a
               => (a -> a -> a)
               -> SPull a
               -> Prog Block (SPush Block a)
reduceLocal f arr
  | len arr == 1 = return $ push arr
  | otherwise    =
    do
      let (a1,a2) = halve arr
      arr' <- unsafeForce $ zipWith f a1 a2
      reduceLocal f arr'

reduce :: MemoryOps a
          => (a -> a -> a)
          -> DPull (SPull a) -> DPush Grid a
reduce f = pConcatMap $ reduceLocal f


input :: DPull EInt32
input = undefinedGlobal (variable "X")

getRed1 = putStrLn $ fst $
          genKernelSpecsNL 1024 "reduce"
                           (reduce (+) . splitUp 2048 :: DPull EInt32 -> DPush Grid EInt32)
performSmall =
  withCUDA $
  do
    kern <- capture 1 (reduce (+) . splitUp 512) 

    useVector (V.fromList [0..1023 :: Int32]) $ \i ->
      useVector (V.fromList [0,0 :: Int32]) $ \ o ->
      do
        o <== (2,kern) <> i 
        r <- peekCUDAVector o
        lift $ putStrLn $ show r 

-- reduceCore :: (Pushable (HLevel (Program t)), Write (Program t), MemoryOps a)
--               => (a -> a -> a)
--               -> SPull a
--               -> Program t (SPull a)

--reduceCore
--  :: (Pushable t, Pushable (HLevel (Prog t)), MemoryOps b,
--      Force (Prog t)) =>
--     (b -> b -> b)
--     -> SPull b -> Prog t (SPush t b)       
reduceCore f arr
  | len arr == 1 = return $ push arr
  | otherwise    =
    do
      let (a1,a2) = halve arr
      arr' <- unsafeForce $ zipWith f a1 a2
      reduceCore f arr'


--reduce' :: forall a t0 . (Pushable t0, Concat (Prog t0 (SPush t0 a)) Grid, MemoryOps a, Force (Prog t0), Pushable (HLevel (Prog t0)))
--          => (a -> a -> a)
--          -> DPull (SPull (SPull a)) -> DPush Grid a
reduce' f arr = pConcatMap (wConcat . fmap (pJoin . reduceCore f)) arr


getRed2 = putStrLn $ fst $
          genKernelSpecsNL 256 "reduce"
                           (reduce' (+) . fmap (splitUp 64) . splitUp 1024 :: DPull EInt32 -> DPush Grid EInt32)

performSmall2 =
  withCUDA $
  do
    kern <- capture 256 (reduce' (+) . fmap (splitUp 128) . splitUp 1024) 

    useVector (V.fromList [0..1023 :: Int32]) $ \i ->
      useVector (V.fromList (P.replicate 8 0)) $ \ o ->
      do
        o <== (1,kern) <> i 
        r <- peekCUDAVector o
        lift $ putStrLn $ show r 

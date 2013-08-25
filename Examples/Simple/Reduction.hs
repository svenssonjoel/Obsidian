
module Reduction where

import Obsidian

import Prelude hiding (zipWith)

import Control.Monad

reduceLocal :: MemoryOps a
               => (a -> a -> a)
               -> SPull a
               -> BProgram (SPull a)
reduceLocal f arr
  | len arr == 1 = return $ arr
  | otherwise    =
    do
      let (a1,a2) = halve arr
      arr' <- unsafeForce $ zipWith f a1 a2
      reduceLocal f arr'

reduce :: MemoryOps a
          => (a -> a -> a)
          -> DPull (SPull a) -> DPush Grid a
reduce f = pConcatMap $ pJoin . liftM push . reduceLocal f 


input :: DPull EInt32
input = undefinedGlobal (variable "X")

--getReduce = putStrLn $ genKernel "reduce" (reduce (+) . splitUp 256) (input :- ())

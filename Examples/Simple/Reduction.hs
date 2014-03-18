{-# LANGUAGE FlexibleContexts #-} 
module Reduction where

import Obsidian

import Prelude hiding (zipWith)

import Control.Monad

reduceLocal :: (Sync t, MemoryOps a, Write t, Pushable t )
               => (a -> a -> a)
               -> SPull a
               -> Program t (SPush t a)
reduceLocal f arr
  | len arr == 1 = return $ push arr
  | otherwise    =
    do
      let (a1,a2) = halve arr
      arr' <- force $ push $ zipWith f a1 a2
      reduceLocal f arr'

local = pJoin

reduceBlocks :: MemoryOps a
          => (a -> a -> a)
          -> SPull a -> SPush Block a
reduceBlocks f arr =
  local $ do
    imm <- force $ pConcat (fmap (reduceLocal f) (splitUp 32 arr))
    reduceLocal f imm
    
reduceGrid :: MemoryOps a
          => (a -> a -> a)
          -> DPull a -> DPush Grid a
reduceGrid f arr = pConcat $ fmap (reduceBlocks f) (splitUp 256 arr) 
    


reduce :: MemoryOps a
          => (a -> a -> a)
          -> DPull a -> DPush Grid a
reduce = reduceGrid

-- reduce :: MemoryOps a
--           => (a -> a -> a)
--           -> DPull a -> DPush Grid a
-- -- reduce f = pConcatMap $ pJoin . liftM push . reduceLocal f 
-- reduce f arr = pConcat (fmap (reduceLocal f) (splitUp 256 arr))

-- reduceExperiment :: MemoryOps a
--                     => (a -> a -> a)
--                     -> DPull a -> DPush Grid a
-- reduceExperiment f arr =
--     pJoin $ do
--       imm <-  force <- pConcat $ fmap (reduceWarps f) (splitUp 256 arr)
--         -- imm :: SPull (SPush a) 
--       undefined -- pConcat (fmap (reduceLocal f) imm )



input :: DPull EInt32
input = undefinedGlobal (variable "X")


-- getReduce = putStrLn $ genKernel "reduce" (reduce (+) . splitUp 256) (input :- ())


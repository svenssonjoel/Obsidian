
module Histogram (histogram, getHistogram) where

import Obsidian

import Prelude

-- This example illustrates a problem with Mutable global arrays.
-- They currently have static size. (problem, not a porblem... i dont know)
-- Also there is a special mutLen function. Not nice, should be len as for any other array.

histogram :: EWord32    -- blocks
             -> EWord32 -- threads
             -> Mutable Global EWord32
             -> DPull EWord32 -> GProgram ()
histogram blocks threads mut arr =
  do
    forAll2 blocks threads $ \bid tid ->
      atomicInc (arr ! (bid * threads + tid)) mut


input :: DPull EWord32
input = undefinedGlobal (variable "X")

inputM :: Mutable Global EWord32
inputM = undefinedMutable (variable "X")

getHistogram = putStrLn $ genKernel "histo" (histogram 256 256) (inputM :- input :- ())


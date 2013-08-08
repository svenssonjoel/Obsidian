
module Histogram where

import Obsidian

import Prelude

-- This example illustrates a problem with Mutable global arrays.
-- They currently have static size. (problem, not a porblem... i dont know)
-- Also there is a special mutLen function. Not nice, should be len as for any other array.

histogram :: Mutable Global EWord32 -> DPull EWord32 -> GProgram ()
histogram mut arr =
  do
    forAll2 b 256 $ \bid tid ->
      atomicInc (arr ! (bid * 256 + tid)) mut
  where
    b = fromIntegral (mutlen mut `div` 256) 


input :: DPull EWord32
input = undefinedGlobal (variable "X")

inputM :: Mutable Global EWord32
inputM = undefinedMutable (variable "X")

getFullHistogram = putStrLn $ genKernel "histo" histogram (inputM :- input :- ())


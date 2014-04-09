
module Histogram (histogram) where 

import Obsidian

import Prelude


histogram :: EWord32    -- blocks
             -> EWord32 -- threads
             -> Mutable Global EWord32 EWord32
             -> DPull EWord32 -> GProgram ()
histogram blocks threads mut arr =
    forAll2 blocks threads $ \bid tid ->
      atomicInc (arr ! (bid * threads + tid)) mut


input :: DPull EWord32
input = undefinedGlobal (variable "X")

inputM :: Mutable Global EWord32 EWord32 
inputM = undefinedMutable (variable "X")

-- getHistogram = putStrLn $ genKernel "histo" (histogram 256 256) (inputM :- input :- ())

         


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


         

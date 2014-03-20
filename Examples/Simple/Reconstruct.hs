
module Reconstruct (reconstruct)  where

import Obsidian

import Prelude



reconstruct :: EWord32
               -> EWord32
               -> DPull EWord32
               -> DPush Grid EWord32
reconstruct blocks threads arr
  = mkPush (blocks * threads) f
  where
    f k =
      forAll2 blocks threads $ \bix tix ->
        let gix = bix * threads + tix
            startIx = arr ! gix
        in  seqFor ((arr ! (gix + 1)) - startIx) $ \ix ->
              k  gix (ix + startIx)

              




module BuildingBlocks where

-- Implement operations on large arrays.
--  TODO: Map
--  TODO: Fold
--  TODO: Permute
--  TODO: Scan
--  Each will probably come in many flavours.
--    + on Distrib or GlobArray
--    + Parameterised in different ways to "tweak" performance. 


import Obsidian.Program
import Obsidian.Array
import Obsidian.Force
import Obsidian.Library

import Data.Word

---------------------------------------------------------------------------
-- MAP
-- 
-- Map on Distrib, Push, Pull and GlobArray is just fmap.
-- But there may ways to decompose a map that is more efficient. 
-- Experiment with sequential work. 
---------------------------------------------------------------------------

mapDSB :: (a -> b)
          -> Word32
          -> Distrib (Pull a) -> Distrib (Pull b)
mapDSB f chunkSize = unchunk . fmap (fmap (fmap f)) . chunk
  where
    chunk   = fmap (sequentially chunkSize) 
    unchunk = fmap (unSequentially chunkSize) 
           
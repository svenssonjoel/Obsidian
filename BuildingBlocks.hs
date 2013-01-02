{-# LANGUAGE ScopedTypeVariables,
             FlexibleContexts,
             GADTs #-}  

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
import Obsidian.Exp
import Obsidian.Types (Type(..))

import Data.Word

---------------------------------------------------------------------------
-- MAP
-- 
-- Map on Distrib, Push, Pull and GlobArray is just fmap.
-- But there may ways to decompose a map that is more efficient. 
-- Experiment with sequential work. 
---------------------------------------------------------------------------

-- mapDSB :: (Forceable (Pull (Seq b)),
--            (Forced (Pull (Seq b)) ~ Pull (Seq b))) -- Huh! getting out of hand
--           => (a -> b)
--           -> Word32
--           -> Distrib (Pull a) -> Distrib (BProgram (Pull b))
-- mapDSB f chunkSize = fmap body 
--   where
--     body arr =
--       do
--         forced <- force $ (fmap (fmap f)) (chunk arr)
--         return (unchunk forced)                  
--     chunk   = sequentially chunkSize
--     unchunk = unSequentially chunkSize
-- Force Instances missing.
-- TODO: Try something like "push" for Sequential arrays
--       (Push elements, Push Seqs of elements)
--       Extend the GlobArray, Push hierarchy "downwards" to thread programs
--       (If at all possible).

---------------------------------------------------------------------------
-- test
---------------------------------------------------------------------------

-- test1 :: Distrib (Pull (Exp Word32)) ->
--          Distrib (BProgram (Pull (Exp Word32)))
-- test1 = mapDSB (+1) 2

-- testInput :: Distrib (Pull (Exp Word32))
-- testInput = namedGlobal "hej" 1 (2*128) 

-- test1' = forceBT . toGlobArray . test1 

-- testPrint = putStrLn$ printPrg $ cheat $ test1' testInput 


---------------------------------------------------------------------------
-- Move to library in some way 
---------------------------------------------------------------------------
toGlobArray :: Distrib (BProgram (Pull a))
               -> GlobArray a               
toGlobArray inp@(Distrib nb bixf) =
  GPush nb bs $
    \wf -> ForAllBlocks nb $
           \bix ->
           do -- BProgram do block 
             arr <- bixf bix 
             ForAll bs $ \ix -> wf (arr ! ix) bix ix 
  where
    bs = len $ fst $ runPrg 0 $ bixf 0
  

forceBT :: forall a. Scalar a => GlobArray (Exp a)
           -> Final (GProgram (Distrib (Pull (Exp a))))
forceBT (GPush nb bs pbt) = Final $ 
  do
      global <- Output $ Pointer (typeOf (undefined :: Exp a))
      
      pbt (assignTo global bs)
        
      return $ Distrib nb  $ 
        \bix -> (Pull bs (\ix -> index global ((bix * (fromIntegral bs)) + ix)))
    where 
      assignTo name s e b i = Assign name ((b*(fromIntegral s))+i) e

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------


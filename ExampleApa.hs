{-# LANGUAGE ScopedTypeVariables,
             FlexibleContexts #-} 

module Examples where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Types
import Obsidian.Array
import Obsidian.Library
import Obsidian.LibraryG
import Obsidian.Force
import Obsidian.CodeGen.Reify
import Obsidian.CodeGen.CompileIM
import Obsidian.CodeGen.Memory
import Obsidian.CodeGen.Liveness
import Text.PrettyPrint.Mainland

import Data.Word
import Data.Int
import Data.Bits
import qualified Data.Map as M

import qualified Data.Vector.Storable as V

import Control.Monad.State

import Prelude hiding (zipWith,sum,replicate,reverse )
import qualified Prelude as P 


mapFusion :: SPull EInt32 -> BProgram (SPush Block EInt32)
mapFusion arr =
  do
    imm <- force $ (fmap (+1) . fmap (*2)) arr
    imm1 <- force $ (fmap (+3) . fmap (*4)) imm
    return $ push imm1

input1 :: DPull EInt32
input1 = namedGlobal "apa" (variable "X")

mapKern :: DPull EInt32 -> DPush Grid EInt32 
mapKern arr = pConcat $ (fmap (pJoin . mapFusion) . splitUp 256)  arr



genKern = ppr $ compile PlatformCUDA (Config 256 1) "apa" (a,rim) 
  where
     (a,im) = toProgram_ 0 mapKern
     iml = computeLiveness im
     (m,mm) = mmIM iml sharedMem (M.empty)
     rim = renameIM mm iml


---------------------------------------------------------------------------
-- Warp experiment
---------------------------------------------------------------------------

-- warpLocal :: SPull EInt32 -> SPush Warp EInt32
-- warpLocal = push . reverse 

-- warpLocal2 :: SPull EInt32 -> Program Warp (SPush Warp EInt32)
-- warpLocal2 arr =
--   do
--     arr1 <- forceWarp $ warpLocal arr
--     arr2 <- forceWarp $ warpLocal arr1
--     return $ warpLocal arr2
             

-- block :: SPull EInt32 -> SPush Block EInt32
-- block arr = wConcat $ fmap warpLocal (splitUp 32 arr)

-- block2 :: SPull EInt32 -> BProgram (SPush Block EInt32)
-- block2 arr =
--   do
--     arr1 <- force $ wConcat $ fmap warpLocal (splitUp 32 arr)
--     return $ wConcat $ fmap  warpLocal (splitUp 32 arr1) 

-- block3 :: SPull EInt32 -> SPush Block EInt32
-- block3 arr = wConcat $ fmap (pJoin . warpLocal2) (splitUp 32 arr)




-- grid :: DPull EInt32 -> DPush Grid EInt32
-- grid arr = pConcat $ fmap block (splitUp 256 arr)

-- grid2 :: DPull EInt32 -> DPush Grid EInt32
-- grid2 arr = pConcat $ fmap (pJoin . block2) (splitUp 256 arr)


-- grid3 :: DPull EInt32 -> DPush Grid EInt32
-- grid3 arr = pConcat $ fmap  block3 (splitUp 256 arr)



-- genGrid = ppr $ compile PlatformCUDA (Config 256 1) "apa" (a,rim) 
--   where
--      (a,im) = toProgram_ 0 grid3
--      iml = computeLiveness im
--      (m,mm) = mmIM iml sharedMem (M.empty)
--      rim = renameIM mm iml

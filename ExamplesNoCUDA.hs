{-# LANGUAGE ScopedTypeVariables,
             FlexibleContexts #-} 

module Examples where

import qualified Obsidian.CodeGen.CUDA as CUDA

import Obsidian.Program
import Obsidian.Exp
import Obsidian.Types
import Obsidian.Array
import Obsidian.Library
import Obsidian.Force
import Obsidian.CodeGen.InOut
import Obsidian.Atomic

import Data.Word
import Data.Int
import Data.Bits

import qualified Data.Vector.Storable as V

import Control.Monad.State

import Prelude hiding (zipWith,sum,replicate)
import qualified Prelude as P 

{-
   -- TODO: Cond finns i Program. Fixa codegen.
   -- TODO: SeqFor finns i Program. Fixa codegen.
   -- Force: bry inte om generalisera nu (eller ngnsin). 
   -- Countingsort: generera kod, se att funkar.
   -- Riktig Countingsort: TODO!
   -- Genererade kernels behöver ibland ta längden av globala arrayer (antal block)
   --     som input. 
-} 

---------------------------------------------------------------------------
-- Util 
---------------------------------------------------------------------------
quickPrint :: ToProgram a b => (a -> b) -> Ips a b -> IO ()
quickPrint prg input =
  putStrLn $ CUDA.genKernel "kernel" prg input 

---------------------------------------------------------------------------
-- MapFusion example
---------------------------------------------------------------------------

mapFusion :: Pull EInt -> BProgram (Pull EInt)
mapFusion arr =
  do
    imm <- force $ (fmap (+1) . fmap (*2)) arr
    force $ (fmap (+3) . fmap (*4)) imm 

input1 :: Pull EInt 
input1 = namedArray "apa" 32

input2 :: GlobPull EInt
input2 = namedGlobal "apa" 

input3 :: GlobPull (Exp Int32)
input3 = namedGlobal "apa" 


---------------------------------------------------------------------------
-- Hacking
---------------------------------------------------------------------------
forAllT' :: GlobPull (Program Thread ()) -> Program Grid ()
forAllT' (GlobPull gixf) = forAllT gixf

forAllLocal :: Pull (Program Thread ()) -> Program Block ()
forAllLocal (Pull n ixf) = ForAll (Just n) ixf 

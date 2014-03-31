

module Obsidian.CodeGen.CUDA (genKernel) where 


import Obsidian.CodeGen.Reify
import Obsidian.CodeGen.CompileIM
import Obsidian.CodeGen.Liveness
import Obsidian.CodeGen.Memory
import Text.PrettyPrint.Mainland

import qualified Data.Map as M
import Data.Word
---------------------------------------------------------------------------
-- Generate CUDA kernels
---------------------------------------------------------------------------

genKernel :: ToProgram prg
             => Word32
             -> String
             -> prg
             -> String
genKernel nt kn prg = prgStr
  where
    prgStr = pretty 75 $ ppr $ compile PlatformCUDA (Config nt bytesShared) kn (a,rim) 
    (a,im) = toProgram_ 0 prg
    iml = computeLiveness im
    (m,mm) = mmIM iml sharedMem (M.empty)
    bytesShared = size m 
    rim = renameIM mm iml

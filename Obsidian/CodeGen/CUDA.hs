
{- Joel Svensson 2013..2017

   Notes:
    25-Nov-2014: Making changes regarding memory allocation
                
-} 

module Obsidian.CodeGen.CUDA (genKernel, genKernelParams, SharedMemConfig(..)) where 


import Obsidian.CodeGen.Reify
import Obsidian.CodeGen.CompileIM
import Obsidian.CodeGen.Liveness
import Obsidian.CodeGen.Memory2
import Text.PrettyPrint.Mainland

import qualified Data.Map as M
import Data.Word
---------------------------------------------------------------------------
-- Generate CUDA kernels
---------------------------------------------------------------------------

-- | Generates kernel C code as a String
--   while assuming there is 48KB of shared mem in 32 banks
genKernel :: ToProgram prg
             => Word32
             -> String
             -> prg
             -> String
genKernel = genKernelParams sm_conf
  where 
    -- pretend we have 32 banks and 48kb shared mem
    sm_conf :: SharedMemConfig 
    sm_conf = SharedMemConfig 49152 32 True



-- | Generates kernel C code as a String,
--   Programmer passes in the Shared memory configuration
genKernelParams :: ToProgram prg
                   => SharedMemConfig
                   -> Word32
                   -> String
                   -> prg
                   -> String
genKernelParams  sm_conf nt kn prg = prgStr
  where
    prgStr = pretty 75
             $ ppr
             $ compileDeclsTop
                (Config nt bytesShared)
                name_loc
                kn (a,im) 
    --         $ compile PlatformCUDA
    --                   (Config nt bytesShared)
    --                   kn (a,rim) 
    (a,im) = toProgram_ 0 prg
    iml = computeLiveness im
    (m,mm) = memMapIM sm_conf iml (M.empty)
    bytesShared = size m
    name_loc = M.assocs mm
    --rim = renameIM mm iml


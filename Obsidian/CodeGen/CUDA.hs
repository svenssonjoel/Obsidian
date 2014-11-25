
{- Joel Svensson 2013, 2014

   Notes:
    25-Nov-2014: Making changes regarding memory allocation
                
-} 

module Obsidian.CodeGen.CUDA (genKernel) where 


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

genKernel :: ToProgram prg
             => Word32
             -> String
             -> prg
             -> String
genKernel nt kn prg = prgStr
  where
    prgStr = pretty 75
             $ ppr
             $ compileDeclsTop
                PlatformCUDA
                (Config nt bytesShared)
                name_loc
                kn (a,im) 
    --         $ compile PlatformCUDA
    --                   (Config nt bytesShared)
    --                   kn (a,rim) 
    (a,im) = toProgram_ 0 prg
    iml = computeLiveness im
    (m,mm) = memMapIM sharedconfig iml (M.empty)
    bytesShared = size m
    name_loc = M.assocs mm
    --rim = renameIM mm iml

    -- pretend we have 32 banks for now.
    -- should be efficiency optimal regardless,
    -- but less space efficient if there are really only 16 banks
    -- TODO: Query device for shared mem size! 
    sharedconfig :: SharedMemConfig 
    sharedconfig = SharedMemConfig 49152  32 True

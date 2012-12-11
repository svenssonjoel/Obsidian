

{- Joel Svensson 2012 -} 
module Obsidian.CodeGen.CUDA.Compile where

import Obsidian.CodeGen.CUDA 

-- This module should interact with the nvcc compiler
-- via the shell


import System.Process

storeAndCompile :: String -> FilePath -> String -> IO FilePath
storeAndCompile arch fp code =
  do
    writeFile fp code
    
    let nfp = fp ++  ".cubin"

    (_,_,_,pid) <-
      createProcess (shell ("nvcc " ++ arch ++ " -cubin -o " ++ nfp ++ " " ++ fp))
    exitCode <- waitForProcess pid
    return nfp


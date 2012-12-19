module Obsidian.Run.CUDA.Exec where



import qualified Foreign.CUDA.Driver as CUDA
import qualified Foreign.CUDA.Driver.Device as CUDA
import qualified Foreign.CUDA.Analysis.Device as CUDA
import qualified Foreign.CUDA.Driver.Stream as CUDAStream


import Obsidian.CodeGen.CUDA
import Obsidian.CodeGen.InOut
import Obsidian.CodeGen.Common (genType,GenConfig(..))
import Obsidian.Types -- experimental 

import qualified Data.Vector.Storable as V
import Foreign.Marshal.Array
import Foreign.ForeignPtr.Unsafe -- (req GHC 7.6 ?) 

import Data.Word
import Data.Supply
import Data.List
import qualified Data.Map as M
import Data.Maybe

import System.IO.Unsafe


{-
  Interface:
   runCUDA
   cudaCapture
   cudaUseVector
   cudaAlloca
   cudaTime
   cudaExecute

-} 

runCUDA = undefined

cudaCapture = undefined
cudaUseVector = undefined
cudaAlloca = undefined
cudaTime = undefined
cudaExecute = undefined

data CUDAVector a = CUDAVector 
---------------------------------------------------------------------------
-- Compile to Cubin (interface with nvcc)
---------------------------------------------------------------------------


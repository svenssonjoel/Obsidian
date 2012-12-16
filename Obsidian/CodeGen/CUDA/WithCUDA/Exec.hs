{-# LANGUAGE TypeOperators,
             GADTs #-}

{- Joel Svensson 2012 -}

module Obsidian.CodeGen.CUDA.WithCUDA.Exec where


import qualified Foreign.CUDA.Driver as CUDA
import qualified Foreign.CUDA.Driver.Device as CUDA
import qualified Foreign.CUDA.Analysis.Device as CUDA
import qualified Foreign.CUDA.Driver.Stream as CUDAStream 

import Obsidian.CodeGen.CUDA.WithCUDA.Compile
import Obsidian.CodeGen.CUDA.WithCUDA

import Obsidian.CodeGen.CUDA
import Obsidian.CodeGen.InOut
import Obsidian.CodeGen.Common (genType,GenConfig(..))
import Obsidian.Types -- experimental 


import Control.Monad.State

import qualified Data.Vector.Storable as V
import Foreign.Marshal.Array
import Foreign.ForeignPtr.Unsafe -- (req GHC 7.6 ?) 


import Data.Word
import Data.Supply
import Data.List
import qualified Data.Map as M
import Data.Maybe

import System.IO.Unsafe


---------------------------------------------------------------------------
-- Get a list of devices from the CUDA driver
---------------------------------------------------------------------------
getDevices :: IO [(CUDA.Device,CUDA.DeviceProperties)]
getDevices = do
  num <- CUDA.count 
  devs <- mapM CUDA.device [0..num-1]
  props <- mapM CUDA.props devs
  return $ zip devs props

---------------------------------------------------------------------------
-- Print a Summary of a device's properties. 
---------------------------------------------------------------------------
propsSummary :: CUDA.DeviceProperties -> String
propsSummary props = unlines
  ["Device Name: " ++ CUDA.deviceName props,
   "Compute Cap: " ++ show (CUDA.computeCapability props),
   "Global Mem:  " ++ show (CUDA.totalGlobalMem props),
   "Shared Mem/Block: " ++ show (CUDA.sharedMemPerBlock props),
   "Registers/Block: "  ++ show (CUDA.regsPerBlock props),
   "Warp Size: " ++ show (CUDA.warpSize props),
   "Max threads/Block: " ++ show (CUDA.maxThreadsPerBlock props),
   "Max threads/MP: " ++ show (CUDA.maxThreadsPerMultiProcessor props),
   "Clock rate: " ++ show (CUDA.clockRate props),
   "Num MP: " ++ show (CUDA.multiProcessorCount props),
   "Mem bus width: " ++ show (CUDA.memBusWidth props)] 

---------------------------------------------------------------------------
-- Environment to run CUDA computations in.
--  # Needs to keep track of generated and loaded functions etc. 
---------------------------------------------------------------------------

data CUDAState = CUDAState { csIdent   :: Integer,
                             csCtx     :: CUDA.Context,
                             csProps   :: CUDA.DeviceProperties,
                             csKernels :: M.Map Integer KernelI,
                             csDptrs   :: M.Map Integer
                                                (CUDA.DevicePtr Word32)}

type CUDA a =  StateT CUDAState IO a

data KernelI  = KernelI {kFun :: CUDA.Fun,
                         kThreadsPerBlock :: Word32 } 

newIdent :: CUDA Integer
newIdent =
  do
    i <- return . csIdent =<< get
    modify (\s -> s {csIdent = i+1 }) 
    return i

withCUDA :: CUDA a -> IO a 
withCUDA p =
  do
    CUDA.initialise []
    devs <- getDevices
    case devs of
      [] -> error "No CUDA device found!" 
      (x:xs) ->
        do 
          ctx <- CUDA.create (fst x) [CUDA.SchedAuto] 
          a <- runStateT p (CUDAState 0 ctx (snd x) M.empty M.empty) 
          CUDA.destroy ctx
          return (fst a)

---------------------------------------------------------------------------
-- Capture and compile a Obsidian function into a CUDA Function
---------------------------------------------------------------------------
capture :: ToProgram a b => (a -> b) -> Ips a b -> CUDA Kernel
capture f inputs =
  do
    i <- newIdent

    props <- return . csProps =<< get
    
    let kn     = "gen" ++ show i
        fn     = kn ++ ".cu"
        cub    = fn ++ ".cubin"
        prgThreads = getNThreads f inputs
        prgstr = genKernel kn f inputs 
        header = "#include <stdint.h>\n" -- more includes ? 
         
    lift $ storeAndCompile (archStr props) (fn) (header ++ prgstr)

    mod <- liftIO $ CUDA.loadFile cub
    fun <- liftIO $ CUDA.getFun mod kn 

    {- After loading the binary into the running process
       can I delete the .cu and the .cu.cubin ? -} 

    m <- return . csKernels =<< get
    modify (\s -> s {csKernels = M.insert i (KernelI fun prgThreads) m})
           
    return $ Kernel i

archStr :: CUDA.DeviceProperties -> String
archStr props = "-arch=sm_" ++ archStr' (CUDA.computeCapability props)
  where
    archStr' (1.0) = "10"
    archStr' (1.2) = "12"
    archStr' (2.0) = "20" 
    archStr' (3.0) = "30"
    -- archStr' x = error $ show x 
    

---------------------------------------------------------------------------
-- useVector: Copies a Data.Vector from "Haskell" onto the GPU Global mem 
--------------------------------------------------------------------------- 
useVector :: V.Storable a =>
             V.Vector a -> (CUDA.DevicePtr a -> CUDA b) -> CUDA b
useVector v f =
  do
    let (hfptr,n) = V.unsafeToForeignPtr0 v
    
    dptr <- lift $ CUDA.mallocArray n
    let hptr = unsafeForeignPtrToPtr hfptr
    lift $ CUDA.pokeArray n hptr dptr
    -- ptrs <- return . csDptrs =<< get
    -- i <- newIdent
    -- modify (\s -> s {csDptrs = M.insert i (CUDA.castDevPtr dptr) ptrs})
    b <- f  dptr     
    lift $ CUDA.free dptr
    return b

copyIn :: V.Storable a =>
          V.Vector a -> CUDA (CUDAVector a)
copyIn v =
  do
    let (hfptr,n) = V.unsafeToForeignPtr0 v
        hptr = unsafeForeignPtrToPtr hfptr
        
    dptr <- lift $ CUDA.mallocArray n
    
    lift $ CUDA.pokeArray n hptr dptr
    ptrs <- return . csDptrs =<< get
    i <- newIdent
    modify (\s -> s {csDptrs = M.insert i (CUDA.castDevPtr dptr) ptrs})
    return $ CUDAVector i 

---------------------------------------------------------------------------
-- allocaVector: allocates room for a vector in the GPU Global mem
---------------------------------------------------------------------------
allocaVector :: V.Storable a => 
                Int -> (CUDA.DevicePtr a -> CUDA b) -> CUDA b
allocaVector n f =
  do
    dptr <- lift $ CUDA.mallocArray n
    b <- f dptr
    lift $ CUDA.free dptr
    return b 


---------------------------------------------------------------------------
-- execute Kernels on the GPU 
---------------------------------------------------------------------------
execute :: (ExecParamList a, ExecParamList b) => Kernel
           -> Word32 -- Number of blocks 
           -> Word32 -- Amount of Shared mem (get from an analysis) 
         --  -> Maybe CUDAStream.Stream
           -> a -> b
           -> CUDA ()
execute (Kernel i)  nb sm {- stream -} a b =
  do
    m <- return . csKernels =<< get
    let k = fromJust$ M.lookup i m
    lift $ CUDA.launchKernel (kFun k)
                             (fromIntegral nb,1,1)
                             (fromIntegral (kThreadsPerBlock k), 1, 1)
                             (fromIntegral sm)
                             Nothing -- stream
                             (toExecParamList a ++ toExecParamList b) -- params

{- 
execute2 :: Kernel
           -> Word32 -- Number of blocks 
           -> Word32 -- Amount of Shared mem (get from an analysis) 
         --  -> Maybe CUDAStream.Stream
           -> [CUDA.VArg]
           -> CUDA ()
execute2 k nb sm params = lift $ 
  CUDA.launchKernel (kFun k)
                    (fromIntegral nb,1,1)
                    (fromIntegral (kThreadsPerBlock k), 1, 1)
                    (fromIntegral sm)
                    Nothing -- stream
                    params
-} 

class ExecParamList a where
  toExecParamList :: a -> [CUDA.FunParam]

instance ExecParamList (CUDA.DevicePtr a) where
  toExecParamList a = [CUDA.VArg a]

instance (ExecParamList a, ExecParamList b) => ExecParamList (a :-> b) where
  toExecParamList (a :-> b) = toExecParamList a ++ toExecParamList b 


---------------------------------------------------------------------------
-- runCUDA
---------------------------------------------------------------------------


runCUDA :: CUDAProgram a -> IO a
runCUDA prg =
  withCUDA $
     runCUDA' prg
 
runCUDA' :: CUDAProgram a -> CUDA a
-- runCUDA' CUDANewId = newIdent
runCUDA' (CUDAKernel f inputs) = capture f inputs 
runCUDA' (CUDAUseVector v) = copyIn v 
  
runCUDA' (CUDATime str prg) = runCUDA' prg
runCUDA' (CUDAExecute i bs sm ins outs) = undefined 
runCUDA' s = error "Not implemented"


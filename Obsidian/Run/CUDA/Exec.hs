{-# LANGUAGE TypeOperators,
             ScopedTypeVariables,
             TypeFamilies,
             TypeSynonymInstances,
             FlexibleInstances #-} 

module Obsidian.Run.CUDA.Exec where

---------------------------------------------------------------------------
--
-- Low level interface to CUDA functionality from Obsidian
--
---------------------------------------------------------------------------


import qualified Foreign.CUDA.Driver as CUDA
import qualified Foreign.CUDA.Driver.Device as CUDA
import qualified Foreign.CUDA.Analysis.Device as CUDA
import qualified Foreign.CUDA.Driver.Stream as CUDAStream

import Obsidian.CodeGen.Reify
import Obsidian.CodeGen.CUDA

import Obsidian.Types -- experimental
import Obsidian.Exp
import Obsidian.Array
import Obsidian.Program (Grid, GProgram)
import Obsidian.Mutable

import Foreign.Marshal.Array
import Foreign.ForeignPtr.Unsafe -- (req GHC 7.6 ?)
import Foreign.ForeignPtr hiding (unsafeForeignPtrToPtr)

import qualified Data.Vector.Storable as V 

import Data.Word
import Data.Int
import Data.Supply
import Data.List
import qualified Data.Map as M
import Data.Maybe

import System.IO.Unsafe
import System.Process
import System.Random.MWC
import System.CPUTime.Rdtsc

import Control.Monad.State



debug = False

---------------------------------------------------------------------------
-- Tools 
---------------------------------------------------------------------------
mkRandomVec :: forall a.(V.Storable a, Variate a) => Int -> IO (V.Vector a)
mkRandomVec k = withSystemRandom $ \g -> uniformVector g k :: IO (V.Vector a) 

---------------------------------------------------------------------------
-- An array located in GPU memory
---------------------------------------------------------------------------
data CUDAVector a = CUDAVector {cvPtr :: CUDA.DevicePtr a,
                                cvLen :: Word32} 

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

--------------------------------------------------------------------------
-- Environment to run CUDA computations in.
--  # Needs to keep track of generated and loaded functions etc. 
---------------------------------------------------------------------------

data CUDAState = CUDAState { csIdent :: Int,
                             csCtx   :: CUDA.Context,
                             csProps :: CUDA.DeviceProperties}

type CUDA a =  StateT CUDAState IO a

data Kernel = Kernel {kFun :: CUDA.Fun,
                      kThreadsPerBlock :: Word32,
                      kSharedBytes :: Word32}

-- Change so that the type parameter to KernelT
-- represents the "captured" type, with CUDAVectors instead of Pull, Push vectors. 
data KernelT a = KernelT {ktFun :: CUDA.Fun,
                          ktThreadsPerBlock :: Word32,
                          ktSharedBytes :: Word32,
                          ktInputs :: [CUDA.FunParam],
                          ktOutput :: [CUDA.FunParam] }

---------------------------------------------------------------------------
-- Kernel Input and Output classes
---------------------------------------------------------------------------
class KernelI a where
  type KInput a 
  addInParam :: KernelT (KInput a -> b) -> a -> KernelT b

class KernelM a where
  type KMutable a
  addMutable :: KernelT (KMutable a -> b) -> a -> KernelT b 
  
class KernelO a where
  type KOutput a 
  addOutParam :: KernelT (KOutput a) -> a -> KernelT ()

instance KernelI Int32 where
  type KInput Int32 = Exp Int32
  addInParam (KernelT f t s i o) a =
    KernelT f t s (i ++ [CUDA.IArg $ fromIntegral a]) o 

instance KernelI Word32 where
  type KInput Word32 = Exp Word32
  addInParam (KernelT f t s i o) a =
    KernelT f t s (i ++ [CUDA.IArg $ fromIntegral a]) o 

instance Scalar a => KernelI (CUDAVector a) where
  type KInput (CUDAVector a) = DPull (Exp a) 
  addInParam (KernelT f t s i o) b =
    KernelT f t s (i ++ [CUDA.VArg (cvPtr b),
                         CUDA.IArg $ fromIntegral (cvLen b)]) o

instance Scalar a => KernelM (CUDAVector a) where
  type KMutable (CUDAVector a) = Mutable Global EW32 (Exp a) 
  addMutable (KernelT f t s i o) b =
    KernelT f t s (i ++ [CUDA.VArg (cvPtr b),
                         CUDA.IArg $ fromIntegral (cvLen b)]) o

instance Scalar a => KernelO (CUDAVector a) where
  type KOutput (CUDAVector a) = DPush Grid (Exp a) 
  addOutParam (KernelT f t s i o) b =
    KernelT f t s i (o ++ [CUDA.VArg (cvPtr b)])

---------------------------------------------------------------------------
-- (<>) apply a kernel to an input
---------------------------------------------------------------------------
(<>) :: KernelI a
        => (Word32,KernelT (KInput a -> b)) -> a -> (Word32,KernelT b)
(<>) (blocks,kern) a = (blocks,addInParam kern a)

---------------------------------------------------------------------------
-- Assign a mutable input/output to a kernel
--------------------------------------------------------------------------- 
(<:>) :: KernelM a
         => (Word32, KernelT (KMutable a -> b)) -> a -> (Word32, KernelT b)
(<:>) (blocks,kern) a = (blocks, addMutable kern a)


---------------------------------------------------------------------------
-- Execute a kernel and store output to an array
---------------------------------------------------------------------------
(<==) :: KernelO b => b -> (Word32, KernelT (KOutput b)) -> CUDA ()
(<==) o (nb,kern) =
  do
    let k = addOutParam kern o
    lift $ CUDA.launchKernel
      (ktFun k)
      (fromIntegral nb,1,1)
      (fromIntegral (ktThreadsPerBlock k), 1, 1)
      (fromIntegral (ktSharedBytes k))
      Nothing -- stream
      (ktInputs k ++ ktOutput k) -- params
    

-- | A variant that returns kernel timing information as well.
-- TODO: We probably can remove this:
-- (<==!) :: KernelO b => b -> (Word32, KernelT (KOutput b)) -> CUDA Word64
-- (<==!) o (nb,kern) =
--   do
--     let k = addOutParam kern o
--     t1 <- lift rdtsc 
--     lift $ CUDA.launchKernel
--       (ktFun k)
--       (fromIntegral nb,1,1)
--       (fromIntegral (ktThreadsPerBlock k), 1, 1)
--       (fromIntegral (ktSharedBytes k))
--       Nothing -- stream
--       (ktInputs k ++ ktOutput k) -- params
--     lift $ CUDA.sync
--     t2 <- lift rdtsc
--     return (t2 - t1) 
syncAll :: CUDA () 
syncAll = lift $ CUDA.sync

-- Tweak these 
infixl 4 <>
infixl 3 <==
-- infixl 3 <==!

---------------------------------------------------------------------------
-- Execute a kernel that has no Output ( a -> GProgram ()) KernelT (GProgram ()) 
---------------------------------------------------------------------------
exec :: (Word32, KernelT (GProgram ())) -> CUDA ()
exec (nb, k) =
 lift $ CUDA.launchKernel
     (ktFun k)
     (fromIntegral nb,1,1)
     (fromIntegral (ktThreadsPerBlock k), 1, 1)
     0 {- (fromIntegral (ktSharedBytes k)) -}
     Nothing -- stream
     (ktInputs k)
---------------------------------------------------------------------------
-- Get a fresh identifier
---------------------------------------------------------------------------
newIdent :: CUDA Int
newIdent =
  do
    i <- gets csIdent
    modify (\s -> s {csIdent = i+1 }) 
    return i


---------------------------------------------------------------------------
-- Run a CUDA computation
---------------------------------------------------------------------------
withCUDA p =
  do
    CUDA.initialise []
    devs <- getDevices
    case devs of
      [] -> error "No CUDA device found!" 
      (x:xs) ->
        do 
          ctx <- CUDA.create (fst x) [CUDA.SchedAuto] 
          (a,_) <- runStateT p (CUDAState 0 ctx (snd x)) 
          CUDA.destroy ctx
          return a

---------------------------------------------------------------------------
-- Capture without an inputlist! 
---------------------------------------------------------------------------    

-- | Compile a program to a CUDA kernel and then load it into memory.
capture :: ToProgram prg
        => Word32  -- ^ Threads per block
        -> prg     -- ^ Program to capture
        -> CUDA (KernelT prg) 
capture threadsPerBlock f =
  do
    i <- newIdent

    props <- return . csProps =<< get
    
    let kn     = "gen" ++ show i
        fn     = kn ++ ".cu"
        cub    = fn ++ ".cubin"

        prgstr = genKernel threadsPerBlock kn f
        header = "#include <stdint.h>\n" -- more includes ? 

    when debug $ 
      do 
        lift $ putStrLn $ prgstr

    let arch = archStr props
        
    lift $ storeAndCompile arch (fn) (header ++ prgstr)
    
    mod <- liftIO $ CUDA.loadFile cub
    fun <- liftIO $ CUDA.getFun mod kn 

    {- After loading the binary into the running process
       can I delete the .cu and the .cu.cubin ? -} 
           
    return $! KernelT fun threadsPerBlock 0 {-bytesShared-} [] []

---------------------------------------------------------------------------
-- useVector: Copies a Data.Vector from "Haskell" onto the GPU Global mem 
--------------------------------------------------------------------------- 
useVector :: V.Storable a =>
             V.Vector a -> (CUDAVector a -> CUDA b) -> CUDA b
useVector v f =
  do
    let (hfptr,n) = V.unsafeToForeignPtr0 v
    
    dptr <- lift $ CUDA.mallocArray n
    let hptr = unsafeForeignPtrToPtr hfptr
    lift $ CUDA.pokeArray n hptr dptr
    let cvector = CUDAVector dptr (fromIntegral (V.length v)) 
    b <- f cvector -- dptr
    lift $ CUDA.free dptr
    return b

---------------------------------------------------------------------------
-- allocaVector: allocates room for a vector in the GPU Global mem
---------------------------------------------------------------------------
allocaVector :: V.Storable a => 
                Int -> (CUDAVector a -> CUDA b) -> CUDA b                
allocaVector n f =
  do
    dptr <- lift $ CUDA.mallocArray n
    let cvector = CUDAVector dptr (fromIntegral n)
    b <- f cvector -- dptr
    lift $ CUDA.free dptr
    return b 

---------------------------------------------------------------------------
-- Allocate and fill with default value
---------------------------------------------------------------------------
allocaFillVector :: V.Storable a => 
                Int -> a -> (CUDAVector a -> CUDA b) -> CUDA b                
allocaFillVector n a f =
  do
    dptr <- lift $ CUDA.mallocArray n
    lift $ CUDA.memset dptr n a 
    let cvector = CUDAVector dptr (fromIntegral n)
    b <- f cvector -- dptr
    lift $ CUDA.free dptr
    return b 

---------------------------------------------------------------------------
-- Fill a Vector
---------------------------------------------------------------------------
fill :: V.Storable a =>
        CUDAVector a -> 
        a -> CUDA ()
fill (CUDAVector dptr n) a =
  lift $ CUDA.memset dptr (fromIntegral n) a 

---------------------------------------------------------------------------
-- Peek in a CUDAVector (Simple "copy back")
---------------------------------------------------------------------------
peekCUDAVector :: V.Storable a => CUDAVector a -> CUDA [a]
peekCUDAVector (CUDAVector dptr n) = 
    lift $ CUDA.peekListArray (fromIntegral n) dptr
    
copyOut :: V.Storable a => CUDAVector a -> CUDA (V.Vector a)
copyOut (CUDAVector dptr n) =
  do
    (fptr :: ForeignPtr a) <- lift $ mallocForeignPtrArray (fromIntegral n)
    let ptr = unsafeForeignPtrToPtr fptr
    lift $ CUDA.peekArray (fromIntegral n) dptr ptr
    return $ V.unsafeFromForeignPtr fptr 0 (fromIntegral n)

---------------------------------------------------------------------------
-- Get the "architecture" of the present CUDA device
---------------------------------------------------------------------------
  
archStr :: CUDA.DeviceProperties -> String
archStr props = "-arch=sm_" ++ archStr' (CUDA.computeCapability props)
  where
    -- Updated for Cuda bindings version 0.5.0.0
    archStr' (CUDA.Compute h l) = show h ++ show l
    --archStr' (CUDA.Compute 1 0) = "10"
    --archStr' (CUDA.Compute 1 2) = "12"
    --archStr' (CUDA.Compute 2 0) = "20" 
    --archStr' (CUDA.Compute 3 0) = "30"
    --archStr' x = error $ "Unknown architecture: " ++ show x 
    
---------------------------------------------------------------------------
-- Compile to Cubin (interface with nvcc)
---------------------------------------------------------------------------
storeAndCompile :: String -> FilePath -> String -> IO FilePath
storeAndCompile arch fp code =
  do
    writeFile fp code
    
    let nfp = fp ++  ".cubin"

    (_,_,_,pid) <-
      createProcess (shell ("nvcc " ++ arch ++ " -cubin -o " ++ nfp ++ " " ++ fp))
    exitCode <- waitForProcess pid
    return nfp

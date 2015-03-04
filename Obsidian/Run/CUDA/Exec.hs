{-# LANGUAGE TypeOperators,
             ScopedTypeVariables,
             TypeFamilies,
             TypeSynonymInstances,
             FlexibleInstances #-} 

module Obsidian.Run.CUDA.Exec ( mkRandomVec
                              , CUDAVector
                              , getDevices
                              , propsSummary
                              , sharedMemConfig
                              , CUDA
                              , KernelT
                              , (<:>)
                              , (<>)
                              , (<==)
                              , syncAll
                              , exec
                              , withCUDA
                              , capture
                              , captureIO
                              , useVector
                              , allocaVector
                              , allocaFillVector
                              , fill
                              , peekCUDAVector
                              , copyOut
                              ) where

---------------------------------------------------------------------------
--
-- Low level interface to CUDA functionality from Obsidian
--
---------------------------------------------------------------------------


import qualified Foreign.CUDA.Driver as CUDA

import Obsidian.CodeGen.Reify
import Obsidian.CodeGen.CUDA

-- import Obsidian.Types -- experimental
import Obsidian.Exp hiding (sizeOf)
import Obsidian.Array
import Obsidian.Program (Grid, GProgram)
import Obsidian.Mutable

-- import Foreign.Marshal.Array
import Foreign.ForeignPtr.Unsafe -- (req GHC 7.6 ?)
import Foreign.ForeignPtr 

import qualified Data.Vector.Storable as V
import Foreign.Storable
import Foreign.Ptr

import Data.Word
import Data.Int
import System.Process
import System.Random.MWC

import Control.Monad.State

debug :: Bool
debug = False

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
instance V.Storable a => V.Storable (Vector4 a) where
  sizeOf _ = sizeOf (undefined :: a) * 4
  alignment _ = alignment (undefined :: a)
 
  {-# INLINE peek #-}
  peek p = do
             a <- peekElemOff q 0
             b <- peekElemOff q 1
             c <- peekElemOff q 2
             d <- peekElemOff q 3
             return (Vector4 a b c d)
    where
      q = castPtr p
  {-# INLINE poke #-}
  poke p (Vector4 a b c d) = do
             pokeElemOff q 0 a
             pokeElemOff q 1 b
             pokeElemOff q 2 c
             pokeElemOff q 3 d
    where
      q = castPtr p

instance V.Storable a => V.Storable (Vector2 a) where
  sizeOf _ = sizeOf (undefined :: a) * 2
  alignment _ = alignment (undefined :: a)
 
  {-# INLINE peek #-}
  peek p = do
             a <- peekElemOff q 0
             b <- peekElemOff q 1
             return (Vector2 a b )
    where
      q = castPtr p
  {-# INLINE poke #-}
  poke p (Vector2 a b) = do
             pokeElemOff q 0 a
             pokeElemOff q 1 b
    where
      q = castPtr p

  


---------------------------------------------------------------------------
-- Tools 
---------------------------------------------------------------------------
-- | Generate a random vector
mkRandomVec :: forall a.(V.Storable a, Variate a) => Int -> IO (V.Vector a)
mkRandomVec k = withSystemRandom $ \g -> uniformVector g k :: IO (V.Vector a) 

---------------------------------------------------------------------------
-- An array located in GPU memory
---------------------------------------------------------------------------
-- | Represents vectors (arrays) in GPU memory.
data CUDAVector a = CUDAVector {cvPtr :: CUDA.DevicePtr a,
                                cvLen :: Word32} 

---------------------------------------------------------------------------
-- Get a list of devices from the CUDA driver
---------------------------------------------------------------------------
-- | Get a list of CUDA devices available in the system 
getDevices :: IO [(CUDA.Device,CUDA.DeviceProperties)]
getDevices = do
  num <- CUDA.count 
  devs <- mapM CUDA.device [0..num-1]
  props <- mapM CUDA.props devs
  return $ zip devs props

---------------------------------------------------------------------------
-- Print a Summary of a device's properties. 
---------------------------------------------------------------------------
-- | A string showing some of the properties os a CUDA device.
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
-- Extract shared mem config
---------------------------------------------------------------------------
-- | Extract shared memory configuration. This information is used
--   by the memory layout manager.
sharedMemConfig :: CUDA.DeviceProperties -> SharedMemConfig 
sharedMemConfig props = SharedMemConfig sm_bytes num_banks True
  where
    sm_bytes = fromIntegral $ CUDA.sharedMemPerBlock props
    num_banks = if x < 2
                then 16
                else 32              
    (CUDA.Compute x _) = CUDA.computeCapability props
    
--------------------------------------------------------------------------
-- Environment to run CUDA computations in.
--  # Needs to keep track of generated and loaded functions etc. 
---------------------------------------------------------------------------
-- | An environment to perform CUDA computations within.
data CUDAState = CUDAState { csIdent :: Int,

                             csCtx   :: CUDA.Context,
                             csProps :: CUDA.DeviceProperties}

type CUDA a =  StateT CUDAState IO a


-- Change so that the type parameter to KernelT
-- represents the "captured" type, with CUDAVectors instead of Pull, Push vectors.

-- | A representation of a CUDA kernel. This represents the
--   kernel after it has been loaded from an Object file. 
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
-- | Apply a kernel to an input 
(<>) :: KernelI a
        => (Word32,KernelT (KInput a -> b)) -> a -> (Word32,KernelT b)
(<>) (blocks,kern) a = (blocks,addInParam kern a)

---------------------------------------------------------------------------
-- Assign a mutable input/output to a kernel
---------------------------------------------------------------------------
-- | apply a kernel to a mutable array 
(<:>) :: KernelM a
         => (Word32, KernelT (KMutable a -> b)) -> a -> (Word32, KernelT b)
(<:>) (blocks,kern) a = (blocks, addMutable kern a)


---------------------------------------------------------------------------
-- Execute a kernel and store output to an array
---------------------------------------------------------------------------
-- | Infix operator taking an allocated output array on LHS and
--   a (Number_Of_Blocks,Kernel) pair on the right. 
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

-- | Wait for all launched work on the device to finish 
syncAll :: CUDA () 
syncAll = lift $ CUDA.sync

-- Tweak these 
infixl 4 <>
infixl 3 <==
-- infixl 3 <==!

---------------------------------------------------------------------------
-- Execute a kernel that has no Output ( a -> GProgram ()) KernelT (GProgram ()) 
---------------------------------------------------------------------------
-- | Execute a kernel that has no specified output.
--   It may write data into a Global Mutable array. 
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
withCUDA :: CUDA a -> IO a
withCUDA p =
  do
    CUDA.initialise []
    devs <- getDevices
    case devs of
      [] -> error "No CUDA device found!" 
      (x:_) ->
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
        --cub    = fn ++ ".cubin"

        sm_conf = sharedMemConfig props
        
        prgstr = genKernelParams sm_conf threadsPerBlock kn f
        header = "#include <stdint.h>\n" -- more includes ? 
      
    
    when debug $ 
      do 
        lift $ putStrLn $ prgstr

    let arch = archStr props
        
    cub_file <- lift $ storeAndCompile arch fn (header ++ prgstr)
    
    cubin <- liftIO $ CUDA.loadFile cub_file
    fun <- liftIO $ CUDA.getFun cubin kn 

    {- After loading the binary into the running process
       can I delete the .cu and the .cu.cubin ? -} 
           
    return $! KernelT fun threadsPerBlock 0 {-bytesShared-} [] []


-- | Compile a program to a CUDA kernel and then load it into memory.
captureIO :: ToProgram prg
             => String  -- ^ name of Kernel (and files stored to disk)
             -> CUDA.DeviceProperties -- ^ deviceproperties to compile for 
             -> Word32  -- ^ Threads per block
             -> prg     -- ^ Program to capture
             -> IO (KernelT prg) 
captureIO nom props threadsPerBlock f =
  do
    let kn     = nom
        fn     = kn ++ ".cu"

        sm_conf = sharedMemConfig props
        
        prgstr = genKernelParams sm_conf threadsPerBlock kn f
        header = "#include <stdint.h>\n" -- more includes ? 
      
    
    when debug $ 
      do 
        putStrLn $ prgstr

    let arch = archStr props
        
    cub_file <- storeAndCompile arch fn (header ++ prgstr)
    
    cubin <- liftIO $ CUDA.loadFile cub_file
    fun <- liftIO $ CUDA.getFun cubin kn 

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
-- | allocate a vector in GPU Device memory 
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
-- | Allocate and Fill a vector in GPU Device memory
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
-- | Fill a vector 
fill :: V.Storable a =>
        CUDAVector a -> 
        a -> CUDA ()
fill (CUDAVector dptr n) a =
  lift $ CUDA.memset dptr (fromIntegral n) a 

---------------------------------------------------------------------------
-- Peek in a CUDAVector (Simple "copy back")
---------------------------------------------------------------------------
-- | Copy a vector from the device to the host
--   as a list.
peekCUDAVector :: V.Storable a => CUDAVector a -> CUDA [a]
peekCUDAVector (CUDAVector dptr n) = 
    lift $ CUDA.peekListArray (fromIntegral n) dptr

-- | Copy a vector from the device to the host.
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
    archStr' (CUDA.Compute h l) = show h ++ show l
    
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
    -- This could fail. Should be error check here (waitForProcess).
    _ <- waitForProcess pid
    return nfp

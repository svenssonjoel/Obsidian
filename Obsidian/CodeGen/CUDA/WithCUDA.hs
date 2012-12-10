{-# LANGUAGE TypeOperators,
             GADTs #-} 
module Obsidian.GCDObsidian.CodeGen.CUDA.WithCUDA where

import qualified Foreign.CUDA.Driver as CUDA
import qualified Foreign.CUDA.Driver.Device as CUDA
import qualified Foreign.CUDA.Analysis.Device as CUDA
import qualified Foreign.CUDA.Driver.Stream as CUDAStream 

import Obsidian.GCDObsidian.CodeGen.CUDA
import Obsidian.GCDObsidian.CodeGen.CUDA.Compile
import Obsidian.GCDObsidian.CodeGen.InOut
import Obsidian.GCDObsidian.CodeGen.Common (genType,GenConfig(..))
import Obsidian.GCDObsidian.Types -- experimental 

import Control.Monad.State

import qualified Data.Vector.Storable as V
import Foreign.Marshal.Array
import Foreign.ForeignPtr.Unsafe -- (req GHC 7.6 ?) 


import Data.Word
import Data.Supply
import Data.List 

import System.IO.Unsafe
import Control.Monad.State

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

data CUDAState = CUDAState { csIdent :: Int,
                             csCtx   :: CUDA.Context,
                             csProps :: CUDA.DeviceProperties}

type CUDA a =  StateT CUDAState IO a

data Kernel = Kernel {kFun :: CUDA.Fun,
                      kThreadsPerBlock :: Word32 } 

newIdent :: CUDA Int
newIdent =
  do
    i <- return . csIdent =<< get
    modify (\s -> s {csIdent = i+1 }) 
    return i
  
withCUDA p =
  do
    CUDA.initialise []
    devs <- getDevices
    case devs of
      [] -> error "No CUDA device found!" 
      (x:xs) ->
        do 
          ctx <- CUDA.create (fst x) [CUDA.SchedAuto] 
          runStateT p (CUDAState 0 ctx (snd x)) 
          CUDA.destroy ctx


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
           
    return $ Kernel fun prgThreads

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
    b <- f dptr     
    lift $ CUDA.free dptr
    return b

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
execute :: (ParamList a, ParamList b) => Kernel
           -> Word32 -- Number of blocks 
           -> Word32 -- Amount of Shared mem (get from an analysis) 
         --  -> Maybe CUDAStream.Stream
           -> a -> b
           -> CUDA ()
execute k nb sm {- stream -} a b = lift $ 
  CUDA.launchKernel (kFun k)
                    (fromIntegral nb,1,1)
                    (fromIntegral (kThreadsPerBlock k), 1, 1)
                    (fromIntegral sm)
                    Nothing -- stream
                    (toParamList a ++ toParamList b) -- params

---------------------------------------------------------------------------
-- ParamList
---------------------------------------------------------------------------

class ParamList a where
  toParamList :: a -> [CUDA.FunParam]

instance ParamList (CUDA.DevicePtr a) where
  toParamList a = [CUDA.VArg a]


instance (ParamList a, ParamList b) => ParamList (a :-> b) where
  toParamList (a :-> b) = toParamList a ++ toParamList b 


---------------------------------------------------------------------------
-- New Approach
---------------------------------------------------------------------------

type Id = Integer

data CUDADir = HostToDevice | DeviceToHost | DeviceToDevice

data CUDAProgram a where
  CUDANewId     :: CUDAProgram Id 
  CUDAKernel    :: String -> CUDAProgram () 
  CUDAUseVector :: (Show a, V.Storable a)
                   => Id 
                   -> V.Vector a
                   -> Type 
                   -> CUDAProgram ()

  CUDACopyVector :: Id -> Id -> Int -> CUDADir -> CUDAProgram ()
  CUDAAllocaVector :: Id 
                      -> Int
                      -> Type 
                      -> CUDAProgram ()

  CUDAExecute :: (String, Word32) 
                 -> Word32 -- Number of blocks
                 -> Word32 -- Amount of Shared mem (get from an analysis) 
                 -> [Id] -- identify inputs.
                 -> [Id] -- identfy outputs. 
                 -> CUDAProgram ()

  --CUDAEventRecord :: CUDAProgram Id -- change type
  --CUDAEventSync   :: Id -> CUDAProgram ()
  --CUDAEventTime   :: Id -> Id -> CUDAProgram Id -- needs improvement. 

  CUDATime :: String -> CUDAProgram () -> CUDAProgram () -- TimeVal  

  -- CUDAShowTimeVal :: TimeVal -> CUDAProgram () 
  
  CUDABind :: CUDAProgram a
              -> (a -> CUDAProgram b)
              -> CUDAProgram b
  CUDAReturn :: a -> CUDAProgram a
  
---------------------------------------------------------------------------
-- Monad Instance
---------------------------------------------------------------------------
instance Monad CUDAProgram where
  return = CUDAReturn
  (>>=)  = CUDABind 

---------------------------------------------------------------------------
--  Wrappers
--------------------------------------------------------------------------- 
cudaCapture :: ToProgram a b => (a -> b) -> Ips a b -> CUDAProgram (String,Word32) 
cudaCapture f inputs =
  do
    id <- CUDANewId
    
    let kn      = "gen" ++ show id
        prgstr  = genKernel kn f inputs
        threads = getNThreads f inputs 
        header  = "#include <stdint.h>\n" -- more includes ? 
         
    CUDAKernel (header ++ prgstr)        
    return (kn,threads)

cudaAlloca :: Int -> Type -> CUDAProgram Id
cudaAlloca size typ =
  do 
    id <- CUDANewId
    CUDAAllocaVector id (size * typeSize typ)  typ
    return id

cudaUseVector :: (Show a, V.Storable a) => V.Vector a -> Type -> CUDAProgram Id
cudaUseVector v typ =
  do
    hostid <- CUDANewId
    devid  <- CUDANewId
    CUDAUseVector hostid v typ
    CUDAAllocaVector devid (V.length v) typ
    CUDACopyVector devid hostid (V.length v * typeSize typ) HostToDevice
    return devid


cudaExecute :: (String, Word32) -> Word32 -> Word32 -> [Id] -> [Id] -> CUDAProgram ()
cudaExecute kern blocks sm ins outs =
  CUDAExecute kern blocks sm ins outs


cudaTime :: String -> CUDAProgram () -> CUDAProgram ()
cudaTime str prg = CUDATime str prg  

---------------------------------------------------------------------------
-- Collect Kernels from a CUDAProgram
---------------------------------------------------------------------------
getKerns :: CUDAProgram a -> [String]
getKerns cp = collectKernels (unsafePerformIO newEnumSupply) cp 

collectKernels :: Supply Id -> CUDAProgram a -> [String]
collectKernels id cp = snd $ collectKernels' id cp 

collectKernels' :: Supply Id -> CUDAProgram a -> (a,[String])
collectKernels' id CUDANewId = (supplyValue id, []) 
collectKernels' id (CUDAKernel str) = ((), [str])
collectKernels' id (CUDABind cp f) =
  let (id1,id2) = split2 id 
      (a,kerns) = collectKernels' id1 cp
      (b,moreKerns) = collectKernels' id2 (f a)
  in  (b,kerns ++ moreKerns)
collectKernels' id (CUDAReturn a) = (a,[])
collectKernels' id (CUDAUseVector i v t) = ((),[])
collectKernels' id (CUDAAllocaVector i s t) = ((),[])
collectKernels' id (CUDAExecute _ _ _ _ _) = ((),[])
collectKernels' id (CUDATime _ p) = collectKernels' id p
--collectKernels' id CUDAEventRecord = (supplyValue id,[])
--collectKernels' id (CUDAEventSync _) = ((),[])
--collectKernels' id (CUDAEventTime _ _) = (supplyValue id,[])

---------------------------------------------------------------------------
-- Output a string with the kernel launch code.
--
--  
---------------------------------------------------------------------------


getCUDA :: CUDAProgram a -> String
getCUDA prg = (concat . intersperse "\n\n") kerns  ++ wrap launcher
  where
    sup = (unsafePerformIO newEnumSupply) 
    (a,kerns,launcher) = getCUDA' sup prg 
    wrap str = "\n\nint main(void){ \n" ++ str ++
               "\n}" 
      
      
getCUDA' :: Supply Id -> CUDAProgram a -> (a, [String], String)
getCUDA' sid CUDANewId           = (supplyValue sid, [], "")
getCUDA' sid (CUDAKernel kern)   = ((),[kern], "")
getCUDA' sid (CUDAUseVector i v t) = ((),[],str)
  where
    -- Should also copy array to device.
    -- Will only work for small arrays right now. 
    str = genType (GenConfig "" "")  t ++
          "v" ++ show i ++ "[" ++ size ++ "] = {"
          ++ dat ++ "};\n"
    size = show $ V.length v
    dat  = concat $ intersperse "," $ map show $ V.toList v
getCUDA' sid (CUDACopyVector o i s d) = ((),[],str)
  where
    str = copy o i s d  
getCUDA' sid (CUDAAllocaVector i s t) = ((),[],str)
  where
    str = allocDeviceVector i s t 
  
getCUDA' sid (CUDAExecute (name,threads) blocks sm ins outs) = ((),[],str)
  where
    str = name++"<<<"++show blocks++","++ show threads ++ "," ++ show sm ++ ">>>" ++
          "(" ++ devIds ins ++ ", " ++ devIds outs ++");\n"
    devIds ids = concat $ intersperse "," $ map (("d"++) . show) ids


getCUDA' sid (CUDATime str prg) = ((),kerns,newstr)
  where 
    newstr = "//Start timing (" ++ str ++ ")\n"  ++ body ++
          "//Stop  timing \n"
    ((),kerns,body) = getCUDA' sid prg 
  
getCUDA' sid (CUDAReturn a) = (a,[],"")
getCUDA' sid (CUDABind cp f) = (b,kerns,launcher) 
  
    where
      (sid1,sid2) = split2 sid
      (a,kerns',launcher') = getCUDA' sid1 cp
      (b,kerns'',launcher'') = getCUDA' sid2 (f a)
      kerns = kerns' ++ kerns''
      launcher = launcher' ++ launcher'' 

copy :: Id -> Id -> Int -> CUDADir-> String
copy o i size d =
  "cudaMemcpy(" ++ dev o ++ "," ++ host i ++ ", "++  show size ++
  ", " ++ dir d ++");\n" 
  where
    host id = "v" ++ show id
    dev  id = "d" ++ show id
    dir HostToDevice = "cudaMemcpyHostToDevice" 
 
allocDeviceVector :: Id -> Int -> Type -> String
allocDeviceVector id size t  =
  typeptr ++ " " ++ nom ++ ";\n" ++  
  malloc nom size  ++ ";\n" 
  where
    nom = "d" ++ show id 
    typeptr = genType (GenConfig "" "") (Pointer t)
    cast str = "(void**)&" ++ str
    malloc nom s = "cudaMalloc("++ cast nom++ ","  ++ show s ++ ")"



typeSize Int8 = 1
typeSize Int16 = 2
typeSize Int32 = 4
typeSize Int64 = 8
typeSize Word8 = 1
typeSize Word16 = 2
typeSize Word32 = 4
typeSize Word64 = 8
typeSize Bool = 4
typeSize Float = 4
typeSize Double = 8 

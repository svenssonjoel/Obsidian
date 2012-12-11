{-# LANGUAGE GADTs #-} 
{- Joel Svensson 2012 -}

module Obsidian.CodeGen.CUDA.WithCUDA.Text where

import Obsidian.CodeGen.CUDA.WithCUDA

import Obsidian.CodeGen.CUDA
import Obsidian.CodeGen.InOut
import Obsidian.CodeGen.Common (genType,GenConfig(..))
import Obsidian.Types -- experimental 

import qualified Data.Vector.Storable as V

import Data.List
import Data.Supply
import qualified Data.Map as M
import Control.Monad.State

import Data.Word

import System.IO.Unsafe
---------------------------------------------------------------------------
-- Collect Kernels from a CUDAProgram
---------------------------------------------------------------------------
{- 
getKerns :: CUDAProgram a -> [String]
getKerns cp = collectKernels (unsafePerformIO newEnumSupply) cp 

collectKernels :: Supply Id -> CUDAProgram a -> [String]
collectKernels id cp = snd $ collectKernels' id cp 

collectKernels' :: Supply Id -> CUDAProgram a -> (a,[String])
collectKernels' id CUDANewId = (supplyValue id, []) 
collectKernels' id (CUDAKernel f input) = (supplyValue id,["ADD KERNEL TO ENV"])  
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
-} 
---------------------------------------------------------------------------
-- Output a string with the kernel launch code.
--
--  
---------------------------------------------------------------------------
getCUDA :: CUDAProgram a -> String
getCUDA prg = (concat . intersperse "\n\n") kerns  ++ wrap launcher
  where
    sup = (unsafePerformIO newEnumSupply) 
    (a,kerns,launcher) = evalState (getCUDA' sup prg) M.empty 
    wrap str = "\n\nint main(void){ \n" ++ str ++
               "\n}" 


data Kernel = Kernel {kString :: String,
                      kThreads :: Word32,
                      kShared  :: Word32} 
type Env = M.Map Id Kernel 

               
getCUDA' :: Supply Id -> CUDAProgram a -> State Env (a, [String], String)
getCUDA' sid CUDANewId           = return (supplyValue sid, [], "")
getCUDA' sid (CUDAKernel f inputs)   =
  do
    let id = supplyValue sid 
    m <- get

    let kn = "gen" ++ show id
        prgstr = genKernel kn f inputs
        threads = getNThreads f inputs
        m' = M.insert id (Kernel prgstr threads 0) m
    put m'
    return (id,[prgstr],"") -- undefined -- ((),[kern], "")

getCUDA' sid (CUDAUseVector i v t) = return ((),[],str)
  where
    -- Should also copy array to device.
    -- Will only work for small arrays right now. 
    str = genType (GenConfig "" "")  t ++
          "v" ++ show i ++ "[" ++ size ++ "] = {"
          ++ dat ++ "};\n"
    size = show $ V.length v
    dat  = concat $ intersperse "," $ map show $ V.toList v
getCUDA' sid (CUDACopyVector o i s d) = return ((),[],str)
  where
    str = copy o i s d  
getCUDA' sid (CUDAAllocaVector i s t) = return ((),[],str)
  where
    str = allocDeviceVector i s t 
  
getCUDA' sid (CUDAExecute kernid blocks sm ins outs) =
  return ((),[],str)
  where
    gah = " <<<LOOKUP KERNEL INFO IN SOME MAP>>> "
    str = gah++"<<<"++show blocks++","++ gah {-show threads-} ++ "," ++ show sm ++ ">>>" ++
          args -- "(" ++ devIds ins ++ ", " ++ devIds outs ++");\n"
    devIds ids = concat $ intersperse "," $ map (("d"++) . show) ids
    args =
      case (ins,outs) of
        ([],_)  -> "(" ++ devIds outs ++ ");\n"
        (_,[])  -> "(" ++ devIds ins ++ ");\n"
        (_,_)   -> "(" ++ devIds ins ++ "," ++ devIds outs ++ ");\n" 

getCUDA' sid (CUDATime str prg) =
  do
    ((),kerns,body) <- getCUDA' sid prg
    let newstr = "//Start timing (" ++ str ++ ")\n"  ++ body ++
                 "//Stop  timing \n"
    
    return ((),kerns,newstr)

getCUDA' sid (CUDAReturn a) = return (a,[],"")
getCUDA' sid (CUDABind cp f) =
  do
    (a,kerns',launcher') <- getCUDA' sid1 cp
    (b,kerns'',launcher'') <- getCUDA' sid2 (f a)
    let kerns = kerns' ++ kerns''
        launcher = launcher' ++ launcher'' 
   
    return (b,kerns,launcher) 
  where
    (sid1,sid2) = split2 sid
   
   
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

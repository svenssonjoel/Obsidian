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
import Data.Maybe

import System.IO.Unsafe

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
        m' = M.insert id (Kernel kn{-prgstr-} threads 0) m
    put m'
    return (id,[prgstr],"") -- undefined -- ((),[kern], "")

--getCUDA' sid (CUDAUseVector i v ) = return ((),[],str)
--  where
    -- Should also copy array to device.
    -- Will only work for small arrays right now. 
--    str = genType (GenConfig "" "")  t ++
--          "v" ++ show i ++ "[" ++ size ++ "] = {"
--          ++ dat ++ "};\n"
--    size = show $ V.length v
--    dat  = concat $ intersperse "," $ map show $ V.toList v
getCUDA' sid (CUDACopyVector o i s d) = return ((),[],str)
  where
    str = copy o i s d  
--getCUDA' sid (CUDAAllocaVector s t f) = return ((),[],str)
--  where
--    str = allocDeviceVector i s t 
  
getCUDA' sid (CUDAExecute kernid blocks sm ins outs) =
  do
    m <- get
    
    let k = fromJust $ M.lookup kernid m
        name = kString k
        threads = kThreads k
        sm = kShared k
        str = name++"<<<"++show blocks++","++ show threads ++ ","
               ++ show sm ++ ">>>" ++ args 
        
    return ((),[],str)
  where
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

getCUDA' sid (CUDAFree id) = return  ((),[],"cudaFree(d"++ show id ++ ");\n")

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

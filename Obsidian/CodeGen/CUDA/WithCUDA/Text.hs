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

import System.IO.Unsafe
---------------------------------------------------------------------------
-- Collect Kernels from a CUDAProgram
---------------------------------------------------------------------------
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
getCUDA' sid (CUDAKernel f input)   = (supplyValue sid,["ADD KERNEL TO ENV"],"") -- undefined -- ((),[kern], "")
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
  
getCUDA' sid (CUDAExecute kernid {- (name,threads)-} blocks sm ins outs) = ((),[],str)
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

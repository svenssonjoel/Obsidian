{- 
   CodeGen.C 
   
   Generate C99 mostly for testing purposes and fun.

   Joel Svensson 2012
-} 


module Obsidian.CodeGen.C where 

import Data.List
import Data.Word 
import Data.Monoid
import qualified Data.Map as Map


import Obsidian.Array 
import Obsidian.Exp  
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Program

import Obsidian.CodeGen.PP
import Obsidian.CodeGen.Common
import Obsidian.CodeGen.InOut
import Obsidian.CodeGen.Liveness
import Obsidian.CodeGen.Memory


---------------------------------------------------------------------------
-- 
gc = genConfig "" ""

    
sBase size = 
  if size > 0 
  then  
    do 
      line "unsigned char *sbase;" 
      newline  
      line ("sbase = (unsigned char*) malloc(" ++ show size ++ ");")
  else
    return () 
    
             
free_sBase size = 
  if size > 0 
  then line "free(sbase);" 
  else return ()

forEach ::  GenConfig -> MemMap -> Exp Word32 -> PP () -> PP ()
forEach gc mm e pp = line ("for (uint32_t tid = 0; tid < " ++ concat (genExp gc mm e) ++"; ++tid)") >>
                     begin >>
                     pp >> 
                     end 

-- TODO: DUPLICATED CODE 
sbaseStr 0 t    = parens$ genCast gc t ++ "sbase" 
sbaseStr addr t = parens$ genCast gc t ++ "(sbase + " ++ show addr ++ ")"

---------------------------------------------------------------------------
-- sequential C code generation

getC :: Config 
        -> Program a 
        -> Name 
        -> [(String,Type)] 
        -> [(String,Type)] 
        -> String 
        
getC conf c name ins outs = 
  runPP (kernelHead name ins outs >>  
         begin >>
         sBase size >> newline >> 
         genCBody conf c >>
         free_sBase size >>
         end ) 0 
  where 
    size = (configLocalMem conf)


---------------------------------------------------------------------------
--
genCBody :: Config -> Program a -> PP () 
genCBody conf prg = genProg mm nt prg
  where
    mm = configMM conf
    nt = configThreads conf

---------------------------------------------------------------------------
-- pretty print a "Program", now C STYLE! 
-- But it is the same ??? 
-- TODO: DUPLICATED CODE 
genProg :: MemMap -> Word32 -> Program a -> PP () 
genProg mm nt (Assign name ix a) = 
  case Map.lookup name mm of 
    Just (addr,t) -> 
      do
        line$  sbaseStr addr t ++ "[" ++ concat (genExp gc mm ix) ++ "] = " ++ 
          concat (genExp gc mm a) ++ ";" 
        newline
    Nothing ->  -- a result array
      do
        line$  name ++ "[" ++ concat (genExp gc mm ix) ++ "] = " ++ 
          concat (genExp gc mm a) ++ ";"
        newline
-- A bit ugly.         
genProg mm nt (ForAll f n) = 
  forEach gc mm (fromIntegral n) (genProg mm nt (f (variable "tid")))
genProg mm nt (Allocate name size t _) = return () 
genProg mm nt Skip = return ()
genProg mm nt (Synchronize _) = return ()
genProg mm nt (ProgramSeq p1 p2) = 
  do 
    genProg mm nt p1
    genProg mm nt p2

---------------------------------------------------------------------------
-- C style function "header"
kernelHead :: Name -> 
              [(String,Type)] -> 
              [(String,Type)] -> 
              PP () 
kernelHead name ins outs = 
  do 
    line ("void " ++ name ++ "(" ++ types ++ ")" )   
  where 
    types = concat (intersperse "," (typeList (ins ++ outs)))
    typeList :: [(String,Type)] -> [String] 
    typeList []              = [] 
    typeList ((a,t):xs)      = (genType gc t ++ a) : typeList xs
  
  
---------------------------------------------------------------------------
-- make "runnable" code 
-- Gives a string that should be a runnable C kernel

genKernel :: (InOut a, InOut b) => String -> (a -> Kernel b) -> a -> String 
genKernel name kernel a = seqc 
  where 
    (input,ins)  = runInOut (createInputs a) (0,[])
  
    ((res,_),c)  = runKernel (kernel input)
    lc = liveness c
   
    threadBudget = 
      case c of 
        Skip -> gcdThreads res
        a  -> threadsNeeded c 
        
    (m,mm) = mapMemory lc sharedMem Map.empty
    (outCode,outs)   = 
      runInOut (writeOutputs threadBudget res) (0,[])
      
    c' = c *>* outCode
    
    seqc = getC (config threadBudget mm (size m)) 
                c' 
                name 
                (("bid",Word32):(map fst2 ins)) 
                (map fst2 outs)
    

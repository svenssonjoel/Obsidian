{- Joel Svensson 2012, 2013 -}

{-# LANGUAGE GADTs #-} 
module Obsidian.CodeGen.CUDA 
       (genKernel) where  

import Data.List
import Data.Word 
import Data.Monoid
import qualified Data.Map as Map
import Control.Monad.State

import Obsidian.Array
import Obsidian.Exp 

import Obsidian.Types
import Obsidian.Globs
import Obsidian.Atomic 

import Obsidian.CodeGen.PP
import Obsidian.CodeGen.Common
import Obsidian.CodeGen.InOut 
import Obsidian.CodeGen.Memory
import Obsidian.CodeGen.Liveness

-- New imports
import Obsidian.CodeGen.Program 
import qualified Obsidian.Program as P 

import Obsidian.CodeGen.SPMDC

---------------------------------------------------------------------------
-- a gc
--------------------------------------------------------------------------- 
gc = genConfig "" ""


---------------------------------------------------------------------------
-- C style function "header"
---------------------------------------------------------------------------

kernelHead :: Name -> 
              [(String,Type)] -> 
              [(String,Type)] -> 
              PP () 
kernelHead name ins outs = 
  do 
    line ("__global__ void " ++ name ++ "(" ++ types ++ ")" )   
  where 
    types = concat (intersperse "," (typeList (ins ++ outs)))
    typeList :: [(String,Type)] -> [String] 
    typeList []              = [] 
    typeList ((a,t):xs)      = (genType gc t ++ a) : typeList xs
 
---------------------------------------------------------------------------
-- genKernel 
---------------------------------------------------------------------------
    
--genKernel :: ToProgram a b => String -> (a -> b) -> Ips a b -> String
genKernel :: ToProgram a => String -> a -> InputList a -> String     
genKernel name kernel a = proto ++ ts ++ cuda 
  where
    (ins,im) = toProgram 0 kernel a

    outs = getOutputs im
    
    lc  = computeLiveness im 
    
    -- Creates (name -> memory address) map      
    (m,mm) = mmIM lc sharedMem Map.empty
             
    -- What if its Right ??? (I DONT KNOW!) 
    (Left threadBudget) = numThreads im
    ts = "/* number of threads needed " ++ show threadBudget ++ "*/\n"

    spmd = imToSPMDC threadBudget im
    
    
    body' = (if size m > 0 then (shared :) else id)  $ mmSPMDC mm spmd

    em = snd $ execState (collectExps body') ( 0, Map.empty)
    --(decls,body'') = replacePass em body'
    --spdecls = declsToSPMDC decls 

    body = body' -- spdecls ++ body''
              
    swap (x,y) = (y,x)
    inputs = map ((\(t,n) -> (typeToCType t,n)) . swap) ins
    outputs = map ((\(t,n) -> (typeToCType t,n)) . swap) outs 
    
    ckernel = CKernel CQualifyerKernel CVoid name (inputs++outputs) body
    shared = CDecl (CQualified CQualifyerExtern (CQualified CQualifyerShared ((CQualified (CQualifyerAttrib (CAttribAligned 16)) (CArray []  (CWord8)))))) "sbase"

    proto = getProto name ins outs 
    cuda = printCKernel (PPConfig "__global__" "" "" "__syncthreads()") ckernel 


---------------------------------------------------------------------------
-- Generate a function prototype
--------------------------------------------------------------------------- 
getProto :: Name -> [(String,Type)] -> [(String,Type)] -> String
getProto name ins outs =
  runPP (
    do 
      line "extern \"C\" "
      kernelHead name ins outs
      line ";"
      newline) 0 

---------------------------------------------------------------------------
-- generate a sbase CExpr
---------------------------------------------------------------------------
sbaseCExpr 0    = cVar "sbase" (CPointer CWord8) 
sbaseCExpr addr = cBinOp CAdd (cVar "sbase" (CPointer CWord8)) 
                              (cLiteral (Word32Val addr) CWord32) 
                              (CPointer CWord8) 
---------------------------------------------------------------------------
-- Memory map the arrays in an SPMDC
---------------------------------------------------------------------------
mmSPMDC :: MemMap -> [SPMDC] -> [SPMDC] 
mmSPMDC mm [] = [] 
mmSPMDC mm (x:xs) = mmSPMDC' mm x : mmSPMDC mm xs

mmSPMDC' :: MemMap -> SPMDC -> SPMDC
mmSPMDC' mm (CAssign e1 es e2) = 
  cAssign (mmCExpr mm e1) 
          (map (mmCExpr mm) es)    
          (mmCExpr mm e2)
mmSPMDC' mm (CAtomic op e1 e2 e3) = cAtomic op (mmCExpr mm e1)
                                               (mmCExpr mm e2)
                                               (mmCExpr mm e3) 
mmSPMDC' mm (CFunc name es) = cFunc name (map (mmCExpr mm) es) 
mmSPMDC' mm CSync           = CSync
mmSPMDC' mm (CIf   e s1 s2) = cIf (mmCExpr mm e) (mmSPMDC mm s1) (mmSPMDC mm s2)
mmSPMDC' mm (CFor name e s) = cFor name (mmCExpr mm e) (mmSPMDC mm s)
mmSPMDC' mm (CWhile b s)    = cWhile (mmCExpr mm b) (mmSPMDC mm s) 
mmSPMDC' mm CBreak = cBreak 
mmSPMDC' mm (CDeclAssign t nom e) = cDeclAssign t nom (mmCExpr mm e)
mmSPMDC' mm a@(CDecl t nom) = a
mmSPMDC' mm a = error $ "mmSPMDC': " ++ show a
---------------------------------------------------------------------------
-- Memory map the arrays in an CExpr
---------------------------------------------------------------------------
mmCExpr mm (CExpr (CVar nom t)) =  
  case Map.lookup nom mm of 
    Just (addr,t) -> 
      let core = sbaseCExpr addr 
          cast c = cCast  c (typeToCType t)
      in cast core
    
    Nothing -> cVar nom t
mmCExpr mm (CExpr (CIndex (e1,es) t)) = cIndex (mmCExpr mm e1, map (mmCExpr mm) es) t
mmCExpr mm (CExpr (CBinOp op e1 e2 t)) = cBinOp op (mmCExpr mm e1) (mmCExpr mm e2) t
mmCExpr mm (CExpr (CUnOp op e t)) = cUnOp op (mmCExpr mm e) t 
mmCExpr mm (CExpr (CFuncExpr nom exprs t)) = cFuncExpr nom (map (mmCExpr mm) exprs) t
mmCExpr mm (CExpr (CCast e t)) = cCast (mmCExpr mm e) t
mmCExpr mm (CExpr (CCond e1 e2 e3 t)) = cCond (mmCExpr mm e1)
                                              (mmCExpr mm e2)
                                              (mmCExpr mm e3)
                                              t
mmCExpr mm a = a 
          
  
---------------------------------------------------------------------------
-- New IM to SPCMD
---------------------------------------------------------------------------
atomicOpToCAtomicOp AtomicInc = CAtomicInc

imToSPMDC :: Word32 -> IMList a -> [SPMDC]
imToSPMDC nt im = concatMap (process nt) im
  where
    process nt (SAssign name [] e,_) =
      [cAssign (cVar name (typeToCType (typeOf e))) [] (expToCExp e)]

    process nt (SAssign name [ix] e,_) = 
      [cAssign (cVar name (typeToCType (Pointer (typeOf e)))) [expToCExp ix] (expToCExp e)]

    process nt (SAtomicOp res arr e op,_) = 
      [cAtomic (atomicOpToCAtomicOp op)
               (cVar res (typeToCType (typeOf e)))
               (cVar arr (typeToCType (Pointer (typeOf e))))
               (expToCExp e)]

    process nt (SCond bexp im,_) =
      [cIf (expToCExp bexp) (imToSPMDC nt im) []]

    process nt (SSeqFor name e im,_) =
      [cFor name (expToCExp e) (imToSPMDC nt im)]
    process nt (SSeqWhile b im,_) =
      [cWhile (expToCExp b) (imToSPMDC nt im)]
    process nt (SBreak,_) =
      [cBreak]

    process nt (SForAll (Literal n) im,_) =
      if (n < nt) 
      then 
        [cIf (cBinOp CLt (cThreadIdx X)  (cLiteral (Word32Val n) CWord32) CInt)
         code []]
      else 
        code 
      where 
        code = imToSPMDC nt im

    -- This one is tricky (since no corresponding CUDA construct exists) 
    process nt (SForAllBlocks n im,_) =
      -- TODO: there should be "number of blocks"-related conditionals here (possibly) 
      imToSPMDC nt im
    -- This one is even more tricky
    process nt (SForAllThreads n im,_) =
      imToSPMDC nt im 
    process nt (SAllocate name size t,_) = []
    process nt (SDeclare name t,_) =
      [cDecl (typeToCType t) name]
    process nt (SOutput name t,_) = [] -- RIGHT!
    process nt (SSynchronize,_)   = [CSync]
    

{- Joel Svensson 2012 -}

{-# LANGUAGE GADTs #-} 
module Obsidian.CodeGen.CUDA 
       (genKernel
       ,genKernel_
       ,getNThreads ) where  

import Data.List
import Data.Word 
import Data.Monoid
import qualified Data.Map as Map

import Obsidian.Array
import Obsidian.Exp 

import Obsidian.Types
import Obsidian.Globs
import Obsidian.Atomic 

import Obsidian.CodeGen.PP
import Obsidian.CodeGen.Common
import Obsidian.CodeGen.InOut 
import Obsidian.CodeGen.SyncAnalysis
import Obsidian.CodeGen.Memory
import Obsidian.CodeGen.Liveness

-- New imports
import Obsidian.CodeGen.Program 
import qualified Obsidian.Program as P 

import Obsidian.CodeGen.SPMDC

{- 
   TODO:
    + Add code generation code for the SeqFor constructor.

    + phase out the old string based codegen 
    + Ideally there should be a Program -> SPMDC
      and SPMDC -> CUDA
          SPMDC -> OpenCL
          SPMDC -> X
      functions. 
-} 

---------------------------------------------------------------------------
-- 
gc = genConfig "" ""

syncLine = line "__syncthreads();"

tidLine = line "unsigned int tid = threadIdx.x;"
bidLine = line "unsigned int bid = blockIdx.x;" 


sBase size = if size > 0
            --     then line "extern __shared__ unsigned char sbase[];"
                then line "extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];" 
                else return ()     
                     
sbaseStr 0 t    = parens$ genCast gc t ++ "sbase" 
sbaseStr addr t = parens$ genCast gc t ++ "(sbase + " ++ show addr ++ ")" 

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
-- getNThreads 
---------------------------------------------------------------------------
getNThreads :: ToProgram a b => (a -> b) -> Ips a b -> Word32
getNThreads kernel a = threadsPerBlock prg
  where
    (ins,prg) = toProgram 0 kernel a
    

---------------------------------------------------------------------------
-- genKernel 
---------------------------------------------------------------------------
genKernel :: ToProgram a b => String -> (a -> b) -> Ips a b -> String 
genKernel name kernel a = proto ++ cuda 
  where
    (ins,prg) = toProgram 0 kernel a

    -- collect outputs and extract the "actual" kernel
    outs = collectOutputs prg
    kern = flatten prg -- extract prg 
    
    lc  = liveness kern -- tmpc
    
    -- Creates (name -> memory address) map      
    (m,mm) = mapMemory lc sharedMem Map.empty

    threadBudget =
      case prg of
        -- The skip case should not really happen
        -- but if we change our mind about that then
        -- it should use sizes of "result" to decide number of threads.
        Skip -> error "empty programs not yet supported"
        a    -> threadsPerBlock prg 

    proto = getProto name ins outs 
    cuda = getCUDA (config threadBudget mm (size m))
                   kern
                   name
                   ins outs
---------------------------------------------------------------------------
-- Generate kernel with optimizations
---------------------------------------------------------------------------
genKernel_ :: ToProgram a b
             => String
              -> (a -> b)
              -> Ips a b
              -> String 
genKernel_ = genKernel' True 

---------------------------------------------------------------------------
-- Generate Kernel via SPMDC with or without optimizations
---------------------------------------------------------------------------
genKernel' :: ToProgram a b
              => Bool
              -> String
              -> (a -> b)
              -> Ips a b
              -> String 
genKernel' doCSE name kernel a = proto ++  cuda
  where
    (ins,prg) = toProgram 0 kernel a
    -- collect outputs and extract the "actual" kernel
    outs = collectOutputs prg
    kern = extract prg 
    lc  = liveness kern 
    (m,mm) = mapMemory lc sharedMem Map.empty
    threadBudget =
      case prg of
        Skip -> error "empty programs not yet supported" 
        a    -> threadsPerBlock prg 

    spmd = progToSPMDC threadBudget kern
    spmd' = if doCSE
            then performCSE2 spmd
            else spmd
    body = shared : mmSPMDC mm spmd'


    swap (x,y) = (y,x)
    inputs = map ((\(t,n) -> (typeToCType t,n)) . swap) ins
    outputs = map ((\(t,n) -> (typeToCType t,n)) . swap) outs 
    
    ckernel = CKernel CQualifyerKernel CVoid name (inputs++outputs) body
    shared = CDecl (CQualified CQualifyerExtern (CQualified CQualifyerShared ((CQualified (CQualifyerAttrib (CAttribAligned 16)) (CArray []  (CWord8)))))) "sbase"

    proto = getProto name ins outs 
    cuda = printCKernel (PPConfig "__global__" "" "" "__syncthreads()") ckernel 

 
---------------------------------------------------------------------------
-- put together all the parts that make a CUDA kernel.
---------------------------------------------------------------------------
getCUDA :: Show a => Config 
          -- -> Code Syncthreads 
           -> Program a 
           -> Name 
           -> [(String,Type)] 
           -> [(String,Type)] 
           -> String 
           
getCUDA conf c name ins outs = 
  runPP (kernelHead name ins outs >>  
         begin >> newline >> 
         -- tidLine >> newline >>
         -- bidLine >> newline >>
         sBase (configLocalMem conf) >> newline >> 
         genCUDABody conf c >>
         end ) 0
  
getProto :: Name -> [(String,Type)] -> [(String,Type)] -> String
getProto name ins outs =
  runPP (
    do 
      line "extern \"C\" "
      kernelHead name ins outs
      line ";"
      newline) 0 

----------------------------------------------------------------------------
-- Code to a CUDA kernel Body
genCUDABody :: Show a=> Config 
              -- -> Code Syncthreads 
               -> Program a 
               -> PP () 
-- genCUDABody _ Skip  = return () 
genCUDABody conf prg = genProg mm nt prg
   where 
      mm = configMM conf
      nt = configThreads conf

---------------------------------------------------------------------------
-- pretty print a "Program", CUDA STYLE!
---------------------------------------------------------------------------
genProg :: Show a => MemMap -> Word32 ->  Program a -> PP () 
genProg mm nt (Assign name ix a) = 
  case Map.lookup name mm of 
    Just (addr,t) -> 
      do
        line$  sbaseStr addr t ++ "[" ++ concat (genExp gc mm ix) ++ "] = " ++ 
          concat (genExp gc mm a) ++ ";" 
        newline
    Nothing ->  --- A result array
      do
        line$  name ++ "[" ++ concat (genExp gc mm ix) ++ "] = " ++ 
          concat (genExp gc mm a) ++ ";"
        newline
--- *** ATOMIC OP CASE 
genProg mm nt (AtomicOp resname name ix AtomicInc) = 
  case Map.lookup name mm of

    -- This case expects an array in shared memory.
    -- atomic ops on shared memory is only allowed on some architectures. 
    Just (addr,t) ->
      do
        -- TODO: Declare a variable with name resname
        --       and type t. Assign result of operation to this var
        line$  "atomicInc(&(" ++ sbaseStr addr t ++ ")" ++ "+"
          ++ concat (genExp gc mm ix) ++ ",0xFFFFFFFF)" ++ ";"
        newline
    -- Lets assume that if name is not in the memory map, then
    -- it is a global memory array. (The problem is that it could
    -- also not be in the memory map because in a bug of generating that map) 
    Nothing ->
      do
        line$  "atomicInc("++ name ++ "+"
          ++ concat (genExp gc mm ix) ++ ",0xFFFFFFFF)" ++ ";"
        newline
genProg mm nt (Cond bexp p) =
  do
    line $ "if " ++ concat (genExp gc mm bexp) ++ "{\n"
    newline
    genProg mm nt p
    newline
    line $ "}\n"
  
                
genProg mm nt (ForAll (Just n) f) = potentialCond gc mm n nt $ 
                                    genProgNoForAll mm nt (f (ThreadIdx X)  ) 
genProg mm nt (ForAll Nothing f) = genProgNoForAll mm nt (f (ThreadIdx X)  )                                   
genProg mm nt (Allocate name size t _) = return () 
genProg mm nt (Synchronize True) = syncLine >> newline 
genProg mm nt (Synchronize False) = return () 
genProg mm nt Skip = return ()
genProg mm nt (ProgramSeq p1 p2) = 
  do 
    genProg mm nt p1
    genProg mm nt p2
genProg mm nt (Output n t) = return () 

-- HACK 
genProgNoForAll :: Show a => MemMap -> Word32 ->  Program a -> PP () 
genProgNoForAll mm nt (Assign name ix a) = 
  case Map.lookup name mm of 
    Just (addr,t) -> 
      do
        line$  sbaseStr addr t ++ "[" ++ concat (genExp gc mm ix) ++ "] = " ++ 
          concat (genExp gc mm a) ++ ";" 
        newline
    Nothing ->  --- A result array
      do
        line$  name ++ "[" ++ concat (genExp gc mm ix) ++ "] = " ++ 
          concat (genExp gc mm a) ++ ";"
        newline
--- *** ATOMIC OP CASE 
genProgNoForAll mm nt (AtomicOp resname name ix AtomicInc) = 
  case Map.lookup name mm of
    Just (addr,t) ->
      do
        -- TODO: Declare a variable with name resname
        --       and type t. Assign result of operation to this var
        line$  "atomicInc(&(" ++ sbaseStr addr t ++ ")" ++ "+"
          ++ concat (genExp gc mm ix) ++ ",0xFFFFFFFF)" ++ ";"
        newline
    -- Lets assume that if name is not in the memory map, then
    -- it is a global memory array. (The problem is that it could
    -- also not be in the memory map because in a bug of generating that map) 
    Nothing ->
      do
        line$  "atomicInc("++ name ++ "+"
          ++ concat (genExp gc mm ix) ++ ",0xFFFFFFFF)" ++ ";"
        newline

genProgNoForAll mm nt (Cond bexp p) =
  do
   -- error $  printPrg p 
    line $ "if " ++ concat (genExp gc mm bexp) ++ "{\n"
    newline
    genProg mm nt p
    newline
    line $ "}\n"
    
    

genProgNoForAll mm nt (SeqFor nom n f) =
  do
    let n' = concat (genExp gc mm n) 
    line$ "for (int "++ nom ++ " =  0;" ++ nom ++ " < " ++ n' ++ ";" ++ nom ++ "++)"
    newline
    begin
    genProgNoForAll mm nt (f (variable nom)) 
    end 
genProgNoForAll mm nt (ForAll n f) = error "genProgNoForAll: Error Program contains nested ForAll" 
genProgNoForAll mm nt (Allocate name size t _) = return () 
genProgNoForAll mm nt (Synchronize True) = syncLine >> newline 
genProgNoForAll mm nt (Synchronize False) = return () 
genProgNoForAll mm nt Skip = return ()
genProgNoForAll mm nt (ProgramSeq p1 p2) = 
  do 
    genProgNoForAll mm nt p1
    genProgNoForAll mm nt p2
genProgNoForAll mm nt (Output n t) = return () 
-- genProgNoForAll mm nt p = error $ printPrg 


---------------------------------------------------------------------------
-- progToSPMDC  
--------------------------------------------------------------------------- 
ctid = cVar "tid" CWord32
  
progToSPMDC :: Word32 -> Program a -> [SPMDC] 
progToSPMDC nt (Assign name ix a) = 
  [cAssign (cVar name CWord8)[expToCExp ix] (expToCExp a)] 
progToSPMDC nt (ForAll (Just n) f) =         
  if (n < nt) 
  then 
    [cIf (cBinOp CLt ctid (cLiteral (Word32Val n) CWord32) CInt)
        code []]
  else 
    code 
  where 
    code = progToSPMDC nt (f (ThreadIdx X))

progToSPMDC nt (ForAll Nothing f) = progToSPMDC nt (f (ThreadIdx X))


progToSPMDC nt (Allocate name size t _) = []
progToSPMDC nt (Synchronize True) = [CSync] 
progToSPMDC nt (Synchronize False) = [] 
progToSPMDC nt Skip = []
progToSPMDC nt (ProgramSeq p1 p2) = progToSPMDC nt p1 ++ progToSPMDC nt p2

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
mmSPMDC' mm (CFunc name es) = cFunc name (map (mmCExpr mm) es) 
mmSPMDC' mm CSync           = CSync
mmSPMDC' mm (CIf   e s1 s2) = cIf (mmCExpr mm e) (mmSPMDC mm s1) (mmSPMDC mm s2)
mmSPMDC' mm (CDeclAssign t nom e) = cDeclAssign t nom (mmCExpr mm e)
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
-- 
---------------------------------------------------------------------------

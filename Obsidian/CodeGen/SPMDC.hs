
{- Joel Svensson 2012,2013 -} 
module Obsidian.CodeGen.SPMDC where


import Obsidian.Globs
import Obsidian.DimSpec

import Obsidian.CodeGen.PP


import Data.Word
import Data.Int

import qualified Data.List as List
import qualified Data.Map as Map

import Control.Monad.State

import Data.Maybe

-- A C LIKE AST (SPMDC - Single Program Multiple Data C) 
{- 
  TODO: 
    + Add for loops to SPMDC 
      - needed for sequential c code generation 
      - potentially also for computing sequentialy on GPUs 
        in the future. (by adding a sequential array construct to Obsidian)

-} 

----------------------------------------------------------------------------
-- 
data Value = IntVal Int         -- allow ? 
           | Int8Val Int8
           | Int16Val Int16
           | Int32Val Int32
           | Int64Val Int64
           | FloatVal Float 
           | DoubleVal Double
           | WordVal   Word     -- allow ? 
           | Word8Val  Word8
           | Word16Val Word16
           | Word32Val Word32
           | Word64Val Word64
           deriving (Eq,Ord,Show)
             
data CType = CVoid | CInt | CFloat | CDouble
           | CInt8 | CInt16 | CInt32 | CInt64 
           | CWord | CWord8 | CWord16 | CWord32 | CWord64
           | CPointer CType -- *type
           | CArray [CExpr] CType -- type[e1][e2][e3]..[en] or type[] 
           | CQualified CQualifyer CType 
           deriving (Eq,Ord,Show)
             
data CQualifyer = CQualifyerGlobal  -- CUDA: ""           OpenCL: "__global" 
                | CQualifyerLocal   -- CUDA: ""           OpenCL: "__local"
                | CQualifyerKernel  -- CUDA: "__global__" OpenCL: "__kernel"  
                | CQualifyerShared  -- CUDA: "__shared__" OpenCL: "__local" 
                | CQualifyerExtern  -- extern   
                | CQualifyerAttrib CQAttribute
                deriving (Eq,Ord,Show)

data CQAttribute = CAttribAligned Word32
                   deriving (Eq,Ord,Show)


data CExprP e  = CVar Name CType 
               -- Threads, Blocks, Grids (All of type Word32) 
               | CBlockIdx  DimSpec 
               | CThreadIdx DimSpec
               | CBlockDim  DimSpec
               | CGridDim   DimSpec
                 
               | CLiteral Value CType
               | CIndex (e,[e]) CType
               | CCond e e e CType
               | CBinOp CBinOp e e  CType
               | CUnOp  CUnOp  e    CType
               | CFuncExpr Name [e] CType  -- min, max, sin, cos 
               | CCast e CType             -- cast expr to type 
               deriving (Eq,Ord,Show)
                        
data CBinOp = CAdd | CSub | CMul | CDiv | CMod  
            | CEq | CNotEq | CLt | CLEq | CGt | CGEq 
            | CAnd | COr
            | CPow
            | CBitwiseAnd | CBitwiseOr | CBitwiseXor 
            | CShiftL | CShiftR 
            deriving (Eq,Ord,Show) 
                     
data CUnOp = CBitwiseNeg
           | CInt32ToWord32
           | CWord32ToInt32
           deriving (Eq,Ord,Show)

{-
   SPMDC and CKernel may turn more complicated if we 
   add features. 
    - loops is an example.. 
       + Already in normal C code generation this will be an issue. 
       
-} 
data SPMDC = CAssign CExpr [CExpr] CExpr  -- array or scalar assign 
           | CDecl CType Name             -- Declare but no assign
           | CDeclAssign CType Name CExpr -- declare variable and assign a value 
           | CFunc   Name  [CExpr]                    
           | CSync                  -- CUDA: "__syncthreads()" OpenCL: "barrier(CLK_LOCAL_MEM_FENCE)"
           | CThreadFence
           | CThreadFenceBlock      -- these could be taken care of with a simple
                                    -- application of the CFunc constructor
                                    -- but since sync,threadfence etc are special
                                    -- and might need attention during code gen
                                    -- I give them specific constructors. 
           | CFor    Name CExpr [SPMDC]  -- very simple loop for now.
           | CIf     CExpr [SPMDC] [SPMDC]
           deriving (Eq,Ord,Show)
                    
--                                ret_t       param list     body
data CKernel = CKernel CQualifyer CType Name [(CType,Name)] [SPMDC] 
             deriving (Eq,Show)
           
----------------------------------------------------------------------------
-- CExpr 
newtype CExpr = CExpr (CExprP CExpr)
             deriving (Eq,Ord,Show)
                      
----------------------------------------------------------------------------                      
-- DAGs
type NodeID = Integer                
newtype CENode = CENode (CExprP NodeID) 
               deriving Show
                        
----------------------------------------------------------------------------
-- Helpers 

cexpr1 exp a       = CExpr $ exp a 
cexpr2 exp a b     = CExpr $ exp a b 
cexpr3 exp a b c   = CExpr $ exp a b c 
cexpr4 exp a b c d = CExpr $ exp a b c d  

cWarpSize  = CExpr $ CVar "warpSize" CWord32 
cBlockIdx  = cexpr1 CBlockIdx
cThreadIdx = cexpr1 CThreadIdx
cBlockDim  = cexpr1 CBlockDim
cGridDim   = cexpr1 CGridDim 
cVar       = cexpr2 CVar 
cLiteral   = cexpr2 CLiteral 
cIndex     = cexpr2 CIndex 
cCond      = cexpr4 CCond   
cFuncExpr  = cexpr3 CFuncExpr 
cBinOp     = cexpr4 CBinOp 
cUnOp      = cexpr3 CUnOp 
cCast      = cexpr2 CCast 

cAssign     = CAssign 
cFunc       = CFunc  
cDecl       = CDecl
cSync       = CSync
cThreadFence = CThreadFence
cThreadFenceBlock = CThreadFenceBlock
cDeclAssign = CDeclAssign 
cIf         = CIf 
cFor        = CFor 
--------------------------------------------------------------------------
-- Printing 
data PPConfig = PPConfig {ppKernelQ :: String, 
                          ppGlobalQ :: String, 
                          ppLocalQ  :: String,
                          ppSyncLine :: String} 

printCKernel :: PPConfig -> CKernel -> String 
printCKernel ppc kern = runPP (ppCKernel ppc  kern ) 0 

ppCKernel :: PPConfig -> CKernel -> PP () 
ppCKernel ppc (CKernel q t nom ins body) = 
  ppCQual ppc q >> space >> ppCType ppc t >> space >> line nom >> ppCommaSepList ppIns "(" ")" ins >> 
  begin >> indent >> newline >> 
  ppSPMDCList ppc body >>  unindent >> newline >>
  end 
  where 
    ppIns (t,nom) = ppCType ppc t >> space >> line nom
  
----------------------------------------------------------------------------
ppCQual ppc CQualifyerGlobal = line$ ppGlobalQ ppc 
ppCQual ppc CQualifyerLocal  = line$ ppLocalQ ppc 
ppCQual ppc CQualifyerKernel = line$ ppKernelQ ppc 
ppCQual ppc CQualifyerExtern = line$ "extern" 
ppCQual ppc CQualifyerShared = line$ "__shared__" -- should this be same as local ?
ppCQual ppc (CQualifyerAttrib a) = ppCAttrib ppc a

ppCAttrib ppc (CAttribAligned x) = line$ "__attribute__ ((aligned(" ++ show x ++ ")))" 
----------------------------------------------------------------------------
ppCType ppc CVoid    = line "void"
ppCType ppc CInt     = line "int"
ppCType ppc CInt8    = line "int8_t"
ppCType ppc CInt16   = line "int16_t"
ppCType ppc CInt32   = line "int32_t"
ppCType ppc CInt64   = line "int64_t"
ppCType ppc CFloat   = line "float"
ppCType ppc CDouble  = line "double"            
ppCType ppc CWord8   = line "uint8_t"
ppCType ppc CWord16  = line "uint16_t"
ppCType ppc CWord32  = line "uint32_t"
ppCType ppc CWord64  = line "uint64_t" 
ppCType ppc (CPointer t) = ppCType ppc t >> line "*"
ppCType ppc (CQualified q t) = ppCQual ppc q >> space >> ppCType ppc t

-- a hack (whats the correct way to handle C's t[] ?)
-- Breaks down already for a[][], i think.
ppCTypedName ppc CVoid   nom = line "void" >> space >> line nom
ppCTypedName ppc CInt    nom = line "int" >> space >> line nom
ppCTypedName ppc CFloat  nom = line "float" >> space >> line nom
ppCTypedName ppc CDouble nom = line "double" >> space >> line nom     
ppCTypedName ppc CWord8  nom = line "uint8_t" >> space >> line nom
ppCTypedName ppc CWord16 nom = line "uint16_t" >> space >> line nom
ppCTypedName ppc CWord32 nom = line "uint32_t" >> space >> line nom
ppCTypedName ppc CWord64 nom = line "uint64_t" >> space >> line nom
ppCTypedName ppc (CPointer t) nom = ppCType ppc t >> line "*" >> line nom
ppCTypedName ppc (CArray [] t) nom = ppCType ppc t >> space >> line nom >> line "[]"
ppCTypedName ppc (CQualified q t) nom = ppCQual ppc q >> space >> ppCTypedName ppc t nom 

----------------------------------------------------------------------------
ppValue (IntVal i)    = line$ show i
ppValue (Int8Val i)   = line$ show i
ppValue (Int16Val i)  = line$ show i
ppValue (Int32Val i)  = line$ show i
ppValue (Int64Val i)  = line$ show i
ppValue (FloatVal f)  = line$ show f 
ppValue (DoubleVal d) = line$ show d
ppValue (Word8Val  w) = line$ show w 
ppValue (Word16Val w) = line$ show w
ppValue (Word32Val w) = line$ show w
ppValue (Word64Val w) = line$ show w 

----------------------------------------------------------------------------
ppBinOp CAdd = line$ "+"
ppBinOp CSub = line$ "-"
ppBinOp CMul = line$ "*"
ppBinOp CDiv = line$ "/"
ppBinOp CMod = line$ "%" 
ppBinOp CEq  = line$ "=="
ppBinOp CLt  = line$ "<" 
ppBinOp CLEq = line$ "<="
ppBinOp CGt  = line$ ">" 
ppBinOp CGEq = line$ ">="
ppBinOp CNotEq = line$ "/=" 
ppBinOp CAnd   = line$ "&&"
ppBinOp COr    = line$ "||" 
ppBinOp CBitwiseAnd = line$ "&"  
ppBinOp CBitwiseOr  = line$ "|" 
ppBinOp CBitwiseXor = line$ "^" 
ppBinOp CShiftL     = line$ "<<" 
ppBinOp CShiftR     = line$ ">>"
                     
ppUnOp CBitwiseNeg = line$ "~"       
-- May be incorrect.
ppUnOp CInt32ToWord32 = line$ "(uint32_t)"
ppUnOp CWord32ToInt32 = line$ "(int32_t)" 

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
ppCommaSepList ppElt s e xs = 
  line s >>  
  sequence_ (List.intersperse (line ",") (commaSepList' xs)) >> line e
  where 
    commaSepList' [] = [] 
    commaSepList' (x:xs) = ppElt x : commaSepList' xs
  
---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
ppSPMDCList ppc xs = sequence_ (map (ppSPMDC ppc) xs) 


ppSPMDC :: PPConfig -> SPMDC -> PP () 
ppSPMDC ppc (CAssign e [] expr) = ppCExpr ppc e >> 
                                  line " = " >> 
                                  ppCExpr ppc expr >> 
                                  cTermLn
ppSPMDC ppc (CAssign e exprs expr) = ppCExpr ppc e >> 
                                     ppCommaSepList (ppCExpr ppc) "[" "]" exprs >> 
                                     line " = " >> 
                                     ppCExpr ppc expr >> 
                                     cTermLn 
--ppSPMDC ppc (CDecl t n) = ppCType ppc t >> space >> line n >> cTermLn
ppSPMDC ppc (CDecl t n) = ppCTypedName ppc t n  >> cTermLn
--ppSPMDC ppc (CDeclAssign t n e) = ppCType ppc t >> space >> line n >> line " = " >> ppCExpr ppc e >> cTermLn
ppSPMDC ppc (CDeclAssign t n e) = ppCTypedName ppc t n >> line " = " >> ppCExpr ppc e >> cTermLn
ppSPMDC ppc (CFunc nom args) = line nom >> ppCommaSepList (ppCExpr ppc) "(" ")" args >> cTermLn
ppSPMDC ppc  CSync = line (ppSyncLine ppc) >> cTermLn 
ppSPMDC ppc (CIf e [] []) = return ()
ppSPMDC ppc (CIf e xs []) = line "if " >> 
                            wrap "(" ")" (ppCExpr ppc e) >> 
                            begin >> indent >> newline  >> 
                            ppSPMDCList ppc xs >>  unindent >> end
ppSPMDC ppc (CIf e xs ys) = line "if " >> 
                            wrap "(" ")" (ppCExpr ppc e) >> 
                            begin >> indent >> newline >> 
                            ppSPMDCList ppc xs >>  unindent >> end >> 
                            line "else " >> begin >> indent >> newline >> 
                            ppSPMDCList ppc ys >>  unindent >> end
-- TODO: Clean up here
ppSPMDC ppc (CFor name e s) = line "for " >>
                              wrap "(" ")" (line ("int " ++ name ++ " = 0;") >>
                                            line (name ++ " < ") >> (ppCExpr ppc e) >>
                                            line (";") >> line "name ++") >>
                              begin >> indent >> newline >> 
                              ppSPMDCList ppc s >> unindent >> end
                            
----------------------------------------------------------------------------
--
ppCExpr :: PPConfig -> CExpr -> PP ()  
-- Cheat and do CUDA print for now!
  -- should do lookup in PPConfig and figure out how to 
  -- print these for CUDA/OpenCL
ppCExpr ppc (CExpr (CBlockIdx X)) = line "blockIdx.x" 
ppCExpr ppc (CExpr (CBlockIdx Y)) = line "blockIdx.y" 
ppCExpr ppc (CExpr (CBlockIdx Z)) = line "blockIdx.z" 
ppCExpr ppc (CExpr (CThreadIdx X)) = line "threadIdx.x" 
ppCExpr ppc (CExpr (CThreadIdx Y)) = line "threadIdx.y" 
ppCExpr ppc (CExpr (CThreadIdx Z)) = line "threadIdx.z" 
ppCExpr ppc (CExpr (CBlockDim X)) = line "blockDim.x" 
ppCExpr ppc (CExpr (CBlockDim Y)) = line "blockDim.y" 
ppCExpr ppc (CExpr (CBlockDim Z)) = line "blockDim.z" 
ppCExpr ppc (CExpr (CGridDim X)) = line "gridDim.x" 
ppCExpr ppc (CExpr (CGridDim Y)) = line "gridDim.y" 
ppCExpr ppc (CExpr (CGridDim Z)) = line "gridDim.z" 

ppCExpr ppc (CExpr (CVar nom _)) = line nom
ppCExpr ppc (CExpr (CLiteral v _)) = ppValue v 
ppCExpr ppc (CExpr (CIndex (e,[]) _)) = ppCExpr ppc e 
ppCExpr ppc (CExpr (CIndex (e,xs) _)) =
  ppCExpr ppc e  >>  
  ppCommaSepList (ppCExpr ppc) "[" "]" xs
ppCExpr ppc (CExpr (CCond e1 e2 e3 _)) =
  wrap "(" ")" 
  (ppCExpr ppc e1 >> 
   line " ? " >> 
   ppCExpr ppc e2 >> 
   line " : " >>  
   ppCExpr ppc e3
  )
ppCExpr ppc (CExpr (CBinOp bop e1 e2 _)) =
  wrap "(" ")"
  (
   ppCExpr ppc e1 >> 
   ppBinOp bop >> 
   ppCExpr ppc e2 
  ) 
ppCExpr ppc (CExpr (CUnOp  uop  e _)) =
  wrap "(" ")" 
  (
   ppUnOp uop >> 
   ppCExpr ppc e 
  )
ppCExpr ppc (CExpr (CFuncExpr nom args _)) =
  line nom >> 
  ppCommaSepList (ppCExpr ppc) "(" ")" args
ppCExpr ppc (CExpr (CCast e t)) =
  line "((" >> 
  ppCType ppc t >> 
  line ")" >> 
  ppCExpr ppc e >> 
  line ")"

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------

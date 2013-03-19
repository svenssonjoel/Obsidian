
{- Joel Svensson 2012,2013 -} 
module Obsidian.CodeGen.SPMDC where

import Obsidian.Globs
import Obsidian.DimSpec

import Obsidian.CodeGen.PP

import Data.Word
import Data.Int

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S 

import Control.Monad.State

import Data.Maybe

-- TODO: Add Atomic ops 

---------------------------------------------------------------------------
-- A C LIKE AST (SPMDC - Single Program Multiple Data C)  
--------------------------------------------------------------------------- 
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
cTypeOfP (CVar _ t) = t
cTypeOfP (CBlockIdx d) = CWord32
cTypeOfP (CThreadIdx d) = CWord32
cTypeOfP (CBlockDim d) = CWord32
cTypeOfP (CGridDim d) = CWord32
cTypeOfP (CLiteral _ t) = t
cTypeOfP (CIndex _ t) = t
cTypeOfP (CCond  _ _ _ t) = t
cTypeOfP (CBinOp _ _ _ t) = t
cTypeOfP (CUnOp _ _ t) = t
cTypeOfP (CFuncExpr _ _ t) = t
cTypeOfP (CCast _ t) = t

cSizeOf (CExpr (CIndex (e,es) _))  = 1 + max (cSizeOf e) (maximum (map cSizeOf es))
cSizeOf (CExpr (CCond e1 e2 e3 _)) = 1 + maximum [cSizeOf e1, cSizeOf e2, cSizeOf e3] 
cSizeOf (CExpr (CFuncExpr _ es _)) = 1 + maximum (map cSizeOf es) 
cSizeOf (CExpr (CUnOp  _ e _)) = 1 + cSizeOf e 
cSizeOf (CExpr (CBinOp _ e1 e2 _ )) = 1+ cSizeOf e1 + cSizeOf e2 
cSizeOf e = 0


data CBinOp = CAdd | CSub | CMul | CDiv | CMod  
            | CEq | CNotEq | CLt | CLEq | CGt | CGEq 
            | CAnd | COr
            | CPow
            | CBitwiseAnd | CBitwiseOr | CBitwiseXor 
            | CShiftL | CShiftR 
            deriving (Eq,Ord,Show) 
                     
data CUnOp = CBitwiseNeg
           deriving (Eq,Ord,Show)

data CAtomicOp = CAtomicAdd | CAtomicInc
               deriving (Eq, Ord, Show) 

---------------------------------------------------------------------------
-- SPMDC
---------------------------------------------------------------------------
data SPMDC = CAssign CExpr [CExpr] CExpr  -- array or scalar assign
           | CAtomic CAtomicOp CExpr CExpr CExpr 
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

cTypeOf (CExpr e) = cTypeOfP e 
                      
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
cAtomic     = CAtomic 
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
--ppUnOp CInt32ToWord32 = line$ "(uint32_t)"
--ppUnOp CWord32ToInt32 = line$ "(int32_t)" 

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
ppCommaSepList ppElt s e xs = 
  line s >>  
  sequence_ (L.intersperse (line ",") (commaSepList' xs)) >> line e
  where 
    commaSepList' [] = [] 
    commaSepList' (x:xs) = ppElt x : commaSepList' xs
  
---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
ppSPMDCList ppc xs = sequence_ (map (ppSPMDC ppc) xs) 


ppSPMDC :: PPConfig -> SPMDC -> PP () 
ppSPMDC ppc (CAssign e [] expr) =
  ppCExpr ppc e >> 
  line " = " >> 
  ppCExpr ppc expr >> 
  cTermLn
ppSPMDC ppc (CAssign e exprs expr) =
  ppCExpr ppc e >> 
  ppCommaSepList (ppCExpr ppc) "[" "]" exprs >> 
  line " = " >> 
  ppCExpr ppc expr >> 
  cTermLn
ppSPMDC ppc (CAtomic op res arr e) =
  --ppCExpr ppc res >>
  --line " = " >>
  ppAtomicOp ppc op >>
  wrap "(" ")" (ppCExpr ppc arr >> line ", " >> ppCExpr ppc e ) >>
  cTermLn 

ppSPMDC ppc (CDecl t n) = ppCTypedName ppc t n  >> cTermLn
ppSPMDC ppc (CDeclAssign t n e) =
  ppCTypedName ppc t n >>
  line " = " >>
  ppCExpr ppc e >> cTermLn
ppSPMDC ppc (CFunc nom args) =
  line nom >>
  ppCommaSepList (ppCExpr ppc) "(" ")" args >> cTermLn
ppSPMDC ppc  CSync = line (ppSyncLine ppc) >> cTermLn 
ppSPMDC ppc (CIf e [] []) = return ()
ppSPMDC ppc (CIf e xs []) =
  line "if " >> 
  wrap "(" ")" (ppCExpr ppc e) >> 
  begin >> indent >> newline  >> 
  ppSPMDCList ppc xs >>  unindent >> end
ppSPMDC ppc (CIf e xs ys) =
  line "if " >> 
  wrap "(" ")" (ppCExpr ppc e) >> 
  begin >> indent >> newline >> 
  ppSPMDCList ppc xs >>  unindent >> end >> 
  line "else " >> begin >> indent >> newline >> 
  ppSPMDCList ppc ys >>  unindent >> end
-- TODO: Clean up here
ppSPMDC ppc (CFor name e s) =
  line "for " >>
  wrap "(" ")" (line ("int " ++ name ++ " = 0;") >>
                line (name ++ " < ") >> (ppCExpr ppc e) >>
                line (";") >> line (name ++ "++")) >>
  begin >> indent >> newline >> 
  ppSPMDCList ppc s >> unindent >> end


ppAtomicOp :: PPConfig -> CAtomicOp -> PP ()
ppAtomicOp ppc CAtomicInc = line "atomicInc" 

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
-- Optimize for complicated indexing expressions
---------------------------------------------------------------------------

-- TODO: #1: Discover all expressions that represent an index into an array
--       #2: Count usages of them
--       #3: For "Complicated" expressions used more than once
--           declare a new name for the index and compute it once. (if not data dependent) 
--
--       Possible approach is two passes over the SPMDC structure.
--       The first discovers expressions
--         The in-between create small SPMDC code that declares variables. 
--       The second replaces some of them by a variable
--

-- Assign with all expressions an integer 
type ExpMap = M.Map CExpr (Int,Int) 

-- Insert, but only if number of occurances and size is right! 
insert :: CExpr -> State (Int,ExpMap) () 
insert e | cSizeOf e >= 2 =
  do
    (i,m) <- get
    case M.lookup e m of
      (Just (id,count)) ->
        do
          let m' = M.insert e (id,count+1) m
          put (i,m')
      Nothing           ->
        do
          let m' = M.insert e (i,1) m
          put (i+1,m')
insert e = return () 

-- Decide if an expression is safe or not to move to
-- function prelude.
-- Simply put it checks for any data dependency.
-- (This code is unused! ) 
safeExp :: S.Set Name -> CExpr -> Bool
safeExp s (CExpr (CVar name _)) = S.member name s
safeExp s (CExpr (CIndex (e,es) _)) = safeExp s e && all (safeExp s) es
safeExp s (CExpr (CCond e1 e2 e3 _)) = safeExp s e1 && safeExp s e2 && safeExp s e3
safeExp s (CExpr (CBinOp _ e1 e2 _)) = safeExp s e2 && safeExp s e2
safeExp s (CExpr (CUnOp _ e _)) = safeExp s e
safeExp s (CExpr (CFuncExpr _ es _)) = all (safeExp s) es
safeExp s (CExpr (CCast e _)) = safeExp s e 
safeExp _ _ = True 
          
collectExps :: [SPMDC] -> State (Int,ExpMap) () 
collectExps sp =  mapM_ process sp
  where
    process (CAssign _ ixs e) =
      do
        mapM_ processE ixs 
        processE e
    process (CDeclAssign _ _ e) = processE e
    process (CFunc _ es) = mapM_ processE es
    process (CFor  _ e sp) =
      do 
        processE e
        collectExps sp
    process (CIf bexp sp1 sp2) =
      do
        processE bexp
        collectExps sp1
        collectExps sp2 
    process a = return () 


    processE (CExpr (CVar _ _))     = return () -- too simple
    processE (CExpr (CBlockIdx d))  = return () 
    processE (CExpr (CThreadIdx d)) = return ()
    processE (CExpr (CBlockDim d))  = return ()
    processE (CExpr (CGridDim d))   = return ()
    processE (CExpr (CLiteral _ _)) = return ()
    processE e@(CExpr (CIndex (e1,es) _)) =
      do 
        -- insert e
        processE e1
        mapM_ processE es
    processE e@(CExpr (CCond e1 e2 e3 _)) =
      do
        insert e
        mapM_ processE [e1,e2,e3]
    processE e@(CExpr (CBinOp _ e1 e2 _)) =
      do
        insert e
        processE e1
        processE e2
    processE e@(CExpr (CUnOp _ e1 _)) =
      do
        insert e
        processE e1
    processE e@(CExpr (CFuncExpr _ es _)) =
      do
        insert e
        mapM_ processE es
    processE e@(CExpr (CCast e1 _)) =
      do
        -- refine this step. Only insert if e1 is nonsimple
        insert e
        processE e1
    

-- REMEMBER TO KEEP IT SIMPLE.
replacePass :: ExpMap -> [SPMDC] -> ([(Int,CExpr)],[SPMDC])
replacePass _ []     = ([],[])
replacePass m (x:xs) = let (decls,x') = process m x
                           (rest, xs') = replacePass m xs
                         
                       in  (L.nubBy fstEq (decls ++ rest), x':xs')
  where
    fstEq :: (Int,a) -> (Int,a) -> Bool
    fstEq a b = fst a == fst b

    process m (CFor name e sp) = (decls,CFor name e' sp')
      where
        (decls1, e') = processE m e
        (decls2, sp') = replacePass m sp
        decls = L.nubBy fstEq (decls1++decls2) 
    process m (CAssign name es e) = (decls,CAssign name es' e')  
      where
        (decls1,es') = processEList m es
        (decls2,e')  = processE m e
        decls = L.nubBy fstEq (decls1 ++ decls2)
    process m s = ([],s)    

    processEList m [] = ([],[])
    processEList m (e:es) =
      let (decls1,e') = processE m e
          (decls2,es') = processEList m es
      in  (L.nubBy fstEq (decls1 ++ decls2),e':es')


    processE m e@(CExpr (CIndex (e1,es) t)) =
      case M.lookup e m of
        Nothing ->
          let (d1,es') = processEList m es
          in (L.nubBy fstEq d1, CExpr (CIndex (e1,es') t))
           
        (Just _) -> error "Just in CIndex case"

    processE m e@(CExpr (CCond e1 e2 e3 t)) =
      case M.lookup e m of
        Nothing ->
          let 
            (d1,e1') = processE m e1
            (d2,e2') = processE m e2
            (d3,e3') = processE m e3
          in (L.nubBy fstEq (d1++d2++d3), CExpr (CCond e1' e2' e3' t))
        Just (id,1) ->
          let 
            (d1,e1') = processE m e1
            (d2,e2') = processE m e2
            (d3,e3') = processE m e3
          in (L.nubBy fstEq (d1++d2++d3), CExpr (CCond e1' e2' e3' t))
        Just (id,n) -> error "SERIOUS FLAW. FIX THIS"
        
    processE m e@(CExpr (CBinOp op e1 e2 t))  =
      case M.lookup e m of
        Nothing -> 
           let (d1,e1') = processE m e1
               (d2,e2') = processE m e2
           in (L.nubBy fstEq (d1++d2), CExpr (CBinOp op e1' e2' t))
             
        (Just (id,1)) -> 
           let (d1,e1') = processE m e1
               (d2,e2') = processE m e2
           in (L.nubBy fstEq (d1++d2), CExpr (CBinOp op e1' e2' t))
          
        (Just (id,n)) -> 
          ([(id,e)],CExpr (CVar ("t" ++ show id) (cTypeOf e)))
          
    processE m e@(CExpr (CCast e1 t)) = (id,CExpr (CCast e1' t))
      where
        (id,e1') = processE m e1 

    processE m e =
      case M.lookup e m of
        Nothing -> ([],e)
        (Just (id,1)) -> ([],e)
        (Just (id,n)) -> ([(id,e)],CExpr (CVar ("t" ++ show id) (cTypeOf e)))
    



declsToSPMDC :: [(Int,CExpr)] -> [SPMDC]
declsToSPMDC decls = map process decls
  where
    process (i,e) = CDeclAssign (cTypeOf e) ("t" ++ show i) e 


{- Joel Svensson 2012 -} 
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

----------------------------------------------------------------------------
--
ppCommaSepList ppElt s e xs = 
  line s >>  
  sequence_ (List.intersperse (line ",") (commaSepList' xs)) >> line e
  where 
    commaSepList' [] = [] 
    commaSepList' (x:xs) = ppElt x : commaSepList' xs
  
----------------------------------------------------------------------------
--
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
ppCExpr ppc (CExpr (CIndex (e,xs) _)) = ppCExpr ppc e  >>  
                                        ppCommaSepList (ppCExpr ppc) "[" "]" xs
ppCExpr ppc (CExpr (CCond e1 e2 e3 _))    = wrap "(" ")" 
                                              (ppCExpr ppc e1 >> 
                                               line " ? " >> 
                                               ppCExpr ppc e2 >> 
                                               line " : " >>  
                                               ppCExpr ppc e3
                                              )
ppCExpr ppc (CExpr (CBinOp bop e1 e2 _)) = line "(" >>  
                                           ppCExpr ppc e1 >> 
                                           ppBinOp bop >> 
                                           ppCExpr ppc e2 >> 
                                           line ")"
ppCExpr ppc (CExpr (CUnOp  uop  e _)) = line "(" >> 
                                        ppUnOp uop >> 
                                        ppCExpr ppc e >> 
                                        line ")" 
ppCExpr ppc (CExpr (CFuncExpr nom args _)) = line nom >> 
                                             ppCommaSepList (ppCExpr ppc) "(" ")" args
ppCExpr ppc (CExpr (CCast e t)) = line "((" >> 
                                  ppCType ppc t >> 
                                  line ")" >> 
                                  ppCExpr ppc e >> 
                                  line ")"


---------------------------------------------------------------------------- 
-- CExpr to Dag and back again. 

{- 
 TODO:  
   + IN PROGRESS: Some things here are clearly faulty. 
     NOTE: fixing this right now by only performing CSE 
       on expressions that can safely be moved to the "head" of the program
     - no regards is taken to scope or code blocks {.. code ... } 
       for example declarations end up within an IF And at the same 
       time the "Computed"-map will say that that variable is computed "globaly"
   + CSE is too brutal. 
      - DONE: I think indexing into a shared memory array should definitely 
              not be stored in a variable. (these two have same access time 
              on the GPU) 

   + Add More detail to the CSEMap. 
      - DONE ALREADY BUT DIFFERENTLY: information about if the declaration of a variable can be moved 
        to toplevel (GLOBAL) or not (LOCAL) 
      - Things are local if they are expressions looking up a value in a shared
        memory array for example or depending on such an expression in any way.   
        Expressions invlving only threadId, BlockId, constants, lengths of global arrays 
        or indexing into global arrays, can be moved to toplevel. (out of all ifs) 
      - Things will be marked as Globally "computed" only if they have been 
        moved out and declared at toplevel.  
      - What about conditional blocks. 
        Atleast Array indexing inside such a block should not be moved. 
        
      


-} 

---------------------------------------------------------------------------- 
-- 
type CSEMap = Map.Map CExpr (NodeID,CENode,Integer)

type Computed = Map.Map NodeID CExpr 


----------------------------------------------------------------------------
newNodeID = do 
  i <- get 
  put (i+1)
  return i

----------------------------------------------------------------------------
insertCM :: CSEMap -> CExpr -> CENode -> State NodeID (CSEMap,NodeID) 
insertCM cm expr node = 
  case Map.lookup expr cm of 
    (Just (i,n,m)) -> 
      -- Already exists in map, just increment usage counter
      let cm' = Map.insert expr (i,n,m+1) cm
      in return (cm', i)
    Nothing  -> do
      -- does not exist in map, add it. 
      i <- newNodeID 
      let cm' = Map.insert expr (i,node,1) cm 
      return (cm',i)
  
----------------------------------------------------------------------------
-- Find expressions that can be computed once globally 

globalName nom = not (List.isPrefixOf "arr" nom)

isGlobal (CExpr (CBlockIdx a)) = True
isGlobal (CExpr (CThreadIdx a)) = True
isGlobal (CExpr (CBlockDim a)) = True
isGlobal (CExpr (CGridDim a)) = True 
isGlobal (CExpr (CVar nom _)) = globalName nom
isGlobal (CExpr (CLiteral l _)) = True 
isGlobal (CExpr (CCast e _)) = isGlobal e
isGlobal (CExpr (CCond e1 e2 e3 _)) = isGlobal e1 && isGlobal e2 && isGlobal e3 

isGlobal (CExpr (CIndex (e,es) _)) = isGlobal e 
  -- Currently the es's will be "global".  
  -- This may change once indexing start depend on "data". (such as in a filter op)  
isGlobal (CExpr (CBinOp _ e1 e2 _)) = isGlobal e1 && isGlobal e2
isGlobal (CExpr (CUnOp _ e _)) = isGlobal e
isGlobal (CExpr (CFuncExpr nom es _)) = all isGlobal es
  
----------------------------------------------------------------------------
cExprToDag :: CSEMap -> CExpr -> State NodeID (CSEMap,NodeID) 
cExprToDag cm exp@(CExpr (CBlockIdx a)) = 
  insertCM cm exp (CENode (CBlockIdx a)) 
cExprToDag cm exp@(CExpr (CThreadIdx a)) = 
  insertCM cm exp (CENode (CThreadIdx a)) 
cExprToDag cm exp@(CExpr (CBlockDim a)) = 
  insertCM cm exp (CENode (CBlockDim a)) 
cExprToDag cm exp@(CExpr (CGridDim a)) = 
  insertCM cm exp (CENode (CGridDim a)) 
  
cExprToDag cm exp@(CExpr (CVar nom t)) = 
  insertCM cm exp (CENode (CVar nom t)) 
cExprToDag cm exp@(CExpr (CLiteral l t)) =  
  insertCM cm exp (CENode (CLiteral l t)) 
cExprToDag cm exp@(CExpr (CCast e t)) = do 
  (cm1,e') <- cExprToDag cm e
  insertCM cm1 exp (CENode (CCast e' t)) 
  
cExprToDag cm exp@(CExpr (CIndex (e,es) t)) = do 
  (cm1,e') <- cExprToDag cm e
  (cm2,es') <- cExprListToDag cm1 es 
  insertCM cm2 exp (CENode (CIndex (e',es') t))

cExprToDag cm exp@(CExpr (CBinOp op e1 e2 t)) = do   
  (cm1,i1) <- cExprToDag cm e1
  (cm2,i2) <- cExprToDag cm1 e2 
  insertCM cm2 exp (CENode (CBinOp op i1 i2 t))

cExprToDag cm exp@(CExpr (CUnOp op e t)) = do    
  (cm1,i1) <- cExprToDag cm e
  insertCM cm1 exp (CENode (CUnOp op i1 t))

cExprToDag cm exp@(CExpr (CFuncExpr nom es t)) = do    
  (cm1,es1) <- cExprListToDag cm es
  insertCM cm1 exp (CENode (CFuncExpr nom es1 t))
cExprToDag cm exp@(CExpr (CCond e1 e2 e3 t)) = do 
  (cm1,e1') <- cExprToDag cm e1 
  (cm2,e2') <- cExprToDag cm1 e2 
  (cm3,e3') <- cExprToDag cm2 e3 
  insertCM cm3 exp (CENode (CCond e1' e2' e3' t))

----------------------------------------------------------------------------
cExprListToDag :: CSEMap -> [CExpr]  -> State NodeID (CSEMap,[NodeID])                  
cExprListToDag cm [] = return (cm,[])
cExprListToDag cm (x:xs) = do 
  (cm', xs') <- cExprListToDag cm xs 
  (cmEnd,x') <- cExprToDag cm' x 
  return (cmEnd, x':xs')

----------------------------------------------------------------------------
type DoneMap = Map.Map CExpr NodeID

----------------------------------------------------------------------------
{-
performCSE :: [SPMDC] -> [SPMDC]
performCSE sp = let (_,_,_,r) = performCSEGlobal Map.empty 0 Map.empty sp
                in r
-}
----------------------------------------------------------------------------
strip = map (\(x,y,_) -> (x,y)) 

{- 
performCSEGlobal :: CSEMap 
                    -> NodeID 
                    -> Computed 
                    -> [SPMDC] 
                    -> (CSEMap,NodeID,Computed,[SPMDC])
performCSEGlobal cm n cp [] = (cm,n,cp,[]) 
performCSEGlobal cm n cp (p:ps) = (cma,nid,cpn,spmdcs ++ prg)
  where 
    (spmdcs,(newnid,cm',cp')) = runState (performCSE' p) (n,cm,cp)
    (cma,nid,cpn,prg) = performCSEGlobal cm' newnid cp' ps
                            
performCSE' :: SPMDC -> State (NodeID,CSEMap,Computed) [SPMDC]
performCSE' CSync = return [CSync]
performCSE' c@(CDeclAssign _ _ _) = return [c]
performCSE' (CAssign nom es e) = do 
  (n,cm,cp) <- get
  let ((cm',nid),n') = runState (cExprToDag cm e) n 
      ((cm'',nids),n'') = buildDagList cm' es n'
      elemList = strip (Map.elems cm'') 
      (cp',decls,newExp) = dagToSPMDC elemList cp nid    
      (cp'',moredecls,exps) = dagListToSPMDC elemList cp' nids
  put (nid+1,cm',cp') 
  return (decls ++ moredecls ++ [CAssign nom exps newExp])
performCSE' (CIf b sp1 sp2) = do 
  (n,cm,cp) <- get
  let ((cm',nid),n') = runState (cExprToDag cm b) n 
      elemList = strip$ Map.elems cm'
      (cp',decls,newExp) = dagToSPMDC elemList cp nid
  put (nid+1,cm',cp') 
  sp1' <- mapM performCSE' sp1 
  sp2' <- mapM performCSE' sp2 
  return$ decls ++ [CIf newExp (concat sp1') (concat sp2')]
performCSE' a@(CFunc nom es) = return [a]
-} 
----------------------------------------------------------------------------
-- 

buildDag cm e n = runState (cExprToDag cm e) n

buildDagList cm [] n = ((cm,[]),n)
buildDagList cm (e:es) n = ((cm'', nid:nids), n'')
  where 
    ((cm',nid),n') = buildDag cm e n
    ((cm'',nids), n'') = buildDagList cm' es n'  


-- TODO: IMPROVE AND CLEAN UP 
dagToSPMDC :: [(NodeID,(CENode,Integer))] 
              -> Computed 
              -> NodeID 
              -> Integer 
              -> Bool 
              -> (Computed,[SPMDC],CExpr)
dagToSPMDC idl cp nid thld b =
  case Map.lookup nid cp of 
    (Just expr) -> (cp,[],expr)
    Nothing -> 
      case lookup nid idl of 
        (Just (CENode (CBlockIdx a),_)) -> (cp, [], cBlockIdx a)
        (Just (CENode (CThreadIdx a),_)) -> (cp, [], cThreadIdx a)
        (Just (CENode (CBlockDim a),_)) -> (cp, [], cBlockDim a)
        (Just (CENode (CGridDim a),_)) -> (cp, [], cGridDim a)
        
        (Just (CENode (CVar nom t),_)) -> (cp,[], cVar nom t)
        (Just (CENode (CLiteral l t),_)) -> (cp,[], cLiteral l t) 
        (Just (CENode (CFuncExpr nom args t),i)) -> 
          if (i > thld || not b)
          then 
            (Map.insert nid newExpr cp1,decs++[newDecl],newExpr )
          else 
            (cp1,decs,inlineExpr) 
          where 
            newExpr = cVar ("imm" ++show nid ) t
            inlineExpr = cFuncExpr nom args' t   -- inline it if its count is not high enough 
            newDecl = cDeclAssign t ("imm" ++ show nid) (cFuncExpr nom args' t) 
            (cp1,decs,args') = dagListToSPMDC idl cp args (max i thld) b -- max is new thld
         
        (Just (CENode (CBinOp op e1 e2 t),i)) -> 
          if ( i > thld || not b) 
          then 
            (Map.insert nid newExpr cp2,decs++[newDecl],newExpr)
          else 
            (cp2, decs, inlineExpr) 
          where 
            newExpr = cVar ("imm" ++ show nid) t
            inlineExpr = cBinOp op e1' e2' t
            newthld = max i thld
            newDecl = cDeclAssign t ("imm" ++ show nid) (cBinOp op e1' e2' t)
            (cp1,d1',e1') = dagToSPMDC idl cp e1  newthld b 
            (cp2,d2',e2') = dagToSPMDC idl cp1 e2 newthld b
            decs = d1' ++ d2'
        (Just (CENode (CUnOp op e t),i)) -> 
          if ( i > thld || not b) 
          then 
            (Map.insert nid newExpr cp1,decs++[newDecl],newExpr)
          else 
            (cp1,decs,inlineExpr) 
          where 
            newExpr = cVar ("imm" ++ show nid) t
            inlineExpr = cUnOp op e' t
            newDecl = cDeclAssign t ("imm" ++ show nid) (cUnOp op e'  t)
            (cp1,d',e') = dagToSPMDC idl cp e (max i thld) b
            decs = d'
       
        -- Do not waste register space for stuff already in shared mem
        (Just (CENode (CIndex (e1,es) t),i)) ->         
          (cp2,decs,cIndex (e1',es') t)
          where 
            --newExpr = cVar ("imm" ++ show nid) t
            --newDecl = cDeclAssign t ("imm" ++ show nid) (cIndex (e1',es') t)
            (cp1,d1',e1') = dagToSPMDC idl cp e1 i b
            (cp2,d2',es')  = dagListToSPMDC idl cp1 es i b 
         
            decs =     d1' ++ d2' 

        (Just (CENode (CCast e t),i)) -> 
          -- Does this do what I hope ?
          (cp',d',newExpr) 
          where 
            newExpr = cCast e' t
            (cp',d',e') = dagToSPMDC idl cp e i b 
        (Just (CENode (CCond e1 e2 e3 t),i)) -> 
          if ( i > thld || not b)
          then
            (Map.insert nid newExpr cp3,decls ++ [newDecl],newExpr)
          else 
            (cp3,decls,inlineExpr) 
          where 
            newExpr = cVar ("imm" ++ show nid) t 
            inlineExpr = cCond e1' e2' e3' t
            newDecl = cDeclAssign t ("imm" ++ show nid) (cCond e1' e2' e3' t)
            (cp1,d1',e1') = dagToSPMDC idl cp e1  i b 
            (cp2,d2',e2') = dagToSPMDC idl cp1 e2 i b
            (cp3,d3',e3') = dagToSPMDC idl cp2 e3 i b
            decls = d1'++d2'++d3'
        Nothing -> error$ "\n" ++ show nid ++ "\n"  ++ show (map fst idl)
          

dagListToSPMDC idl cp [] i b = (cp,[],[])
dagListToSPMDC idl cp (x:xs) i b = (cp'',decs ++ moredecs, exp:exps)
  where 
    (cp',decs,exp) = dagToSPMDC idl cp x i b 
    (cp'',moredecs, exps) = dagListToSPMDC idl cp' xs i b

snd3 (_,y,_) = y
trd3 (_,_,z) = z

----------------------------------------------------------------------------
-- 

buildCSEMap :: [SPMDC] -> CSEMap 
buildCSEMap sps = snd$  buildCSEMap' (Map.empty) 0 sps 
  where 
    
buildCSEMap' cm n [] = (n,cm) 
buildCSEMap' cm n (sp:sps) =  buildCSEMap' cm' n' sps
  where 
    (n',cm') = (collectCSE cm n sp)

    
collectCSE cm n CSync = (n,cm)
collectCSE cm n (CDeclAssign _ _ _) = error "CDeclAssign found during collectCSE"
collectCSE cm n (CAssign nom es e) = 
  let ((cm',nid),n') = runState (cExprToDag cm e) n 
      ((cm'',nids),n'') = buildDagList cm' es n'
  in (n'',cm'')
     

collectCSE cm n (CIf b sp1 sp2) = (n3,cm3)
  where 
    ((cm1,nid),n1) = runState (cExprToDag cm b) n 
    (n2,cm2) = buildCSEMap' cm1 n1 sp1
    (n3,cm3) = buildCSEMap' cm2 n2 sp2 
collectCSE cm n (CFunc nom es) = (n1,cm1)
  where 
    ((cm1,nids),n1) = buildDagList cm es n


----------------------------------------------------------------------------
-- 2nd performCSE experiment
performCSE2 :: [SPMDC] -> [SPMDC] 
performCSE2 sps = globDecls ++ r
  where 
    cseMap = buildCSEMap sps -- map containing all expressions 
   
    (cp,globDecls) = declareGlobals cseMap
    
    r = performCSEPass cseMap cp sps 
        
   
        
    -- r' = performCSE r 
    
    
---------------------------------------------------------------------------- 
-- 
declareGlobals :: CSEMap -> (Computed, [SPMDC]) 
declareGlobals cm = declareGlobals' (Map.empty) globs 
  where 
    getGlobals :: CSEMap -> [(NodeID,CENode,Integer)]
    getGlobals cm = globs
      where 
        globs = Map.elems cm' 
        cm'   = Map.filterWithKey (\k e -> isGlobal k) cm
    -- Alternative approach to getting globals would be 
    -- to look at the expressions in the program (not the map).
    -- only adding "topmost level" expressions. that is 
    -- only stepping into an expression if the isGlobal evaluates
    -- to false on it and potentially add subexpressions. 
        
    globs = getGlobals cm 
    strip = map (\(x,y,z) -> (x,y))
    declareGlobals' cp []  = (cp,[]) 
    declareGlobals' cp ((nid,cenode,i):xs) = 
      if (i >= 2) 
      then
        case Map.lookup nid cp of
          (Just e) -> declareGlobals' cp xs          
          Nothing -> 
            let (cp',sps,e) = dagToSPMDC (pairup globs) cp nid  0 False
                (cp'',sps2) = declareGlobals' cp' xs
            in (cp'',sps ++ sps2)
      else declareGlobals' cp xs          
      where 
        pairup = map (\(x,y,z) -> (x,(y,z)))
           
           
      
----------------------------------------------------------------------------
-- PerformCSEPass 
-- Walk over code and replace expressions with globaly defined variables
performCSEPass :: CSEMap -> Computed -> [SPMDC] -> [SPMDC]                             
performCSEPass cm cp [] = []
performCSEPass cm cp (x:xs) = performCSEPass' cm cp x : performCSEPass cm cp xs 

-- Does not add any new declarations (Maybe will later)              
--  + TODO: Perform local CSE on things that can not be "moved". 
--       - inside If blocks 
--       - can rely on intermediate arrays all having separate names.
--         So not much need to track scope very much 

performCSEPass' :: CSEMap -> Computed -> SPMDC -> SPMDC
performCSEPass' cm cp CSync = CSync
performCSEPass' cm cp (CDeclAssign _ _ _) = error "performCSEPass': CDeclAssign found during CSEPass" 
performCSEPass' cm cp (CDecl _ _)         = error "performCSEPass': CDecl found during CSEPass" 
performCSEPass' cm cp (CAssign nom es e)  = CAssign nom xs x 
  where
    (x:xs) = cseReplaceL cm cp (e:es) 
performCSEPass' cm cp (CIf b sp1 sp2) = CIf b' (performCSEPass cm cp sp1) 
                                               (performCSEPass cm cp sp2)
  where 
    b' = cseReplace cm cp b 
performCSEPass' cm cp a@(CFunc nom es) = a -- look


----------------------------------------------------------------------------
cseReplaceL cm cp [] = []
cseReplaceL cm cp (x:xs) = cseReplace cm cp x: cseReplaceL cm cp xs


cseReplace cm cp exp@(CExpr (CIndex (e,es) t))  = 
  case Map.lookup exp cm of 
    (Just (nid,node,_)) ->
      case Map.lookup nid cp of 
        (Just exp') -> exp' 
        Nothing -> CExpr (CIndex (cseReplace cm cp e,
                                  cseReplaceL cm cp es) t)
    Nothing -> error "cseReplace: expression missing from CSEMap"
cseReplace cm cp exp@(CExpr (CCast e t)) = 
  case Map.lookup exp cm of 
    (Just (nid,node,_)) ->
      case Map.lookup nid cp of 
        (Just exp') -> exp' 
        Nothing -> CExpr (CCast (cseReplace cm cp e) t)                                 
    Nothing -> error "cseReplace: expression missing from CSEMap"
cseReplace cm cp exp@(CExpr (CBinOp op e1 e2 t)) = 
  case Map.lookup exp cm of 
    (Just (nid,node,_)) ->
      case Map.lookup nid cp of 
        (Just exp') -> exp' 
        Nothing -> CExpr (CBinOp op (cseReplace cm cp e1) 
                                    (cseReplace cm cp e2) t)                                 
    Nothing -> error "cseReplace: expression missing from CSEMap"                                        
cseReplace cm cp exp@(CExpr (CUnOp op e t)) = 
  case Map.lookup exp cm of 
    (Just (nid,node,_)) ->
      case Map.lookup nid cp of 
        (Just exp') -> exp' 
        Nothing -> CExpr (CUnOp op (cseReplace cm cp e) t)                                 
    Nothing -> error "cseReplace: expression missing from CSEMap"                                        
cseReplace cm cp exp@(CExpr (CFuncExpr nom es t)) = 
  case Map.lookup exp cm of 
    (Just (nid,node,_)) ->
      case Map.lookup nid cp of 
        (Just exp') -> exp' 
        Nothing -> CExpr (CFuncExpr nom (cseReplaceL cm cp es) t)                                 
    Nothing -> error "cseReplace: expression missing from CSEMap"                                        
cseReplace cm cp exp@(CExpr (CCond e1 e2 e3 t)) =     
  case Map.lookup exp cm of 
    (Just (nid,node,_)) -> 
      case Map.lookup nid cp of 
        (Just exp') -> exp'
        Nothing -> CExpr (CCond (cseReplace cm cp e1) 
                                (cseReplace cm cp e2) 
                                (cseReplace cm cp e3) t)
-- dont know what to do so just put expression back...     
cseReplace cm cp exp = 
  case Map.lookup exp cm of 
    (Just (nid,node,_)) ->  
      case Map.lookup nid cp of 
        (Just exp') -> exp'
        Nothing     -> exp 
    Nothing -> error "cseReplace: expression missing from CSEMap"                                        
    
----------------------------------------------------------------------------


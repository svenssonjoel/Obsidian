{-# LANGUAGE RankNTypes, GADTs  #-}

{- Joel Svensson 2012 -} 
module Obsidian.CodeGen.Common where 

import Data.List
import Data.Word
import qualified Data.Map as Map 


import Obsidian.Exp 
import Obsidian.Types
import Obsidian.Globs

import Obsidian.CodeGen.PP
import Obsidian.CodeGen.Memory

---------------------------------------------------------------------------
data GenConfig = GenConfig { global :: String,
                             local  :: String };
  
genConfig = GenConfig


---------------------------------------------------------------------------
-- Helpers

mappedName :: Name -> Bool 
mappedName name = isPrefixOf "arr" name

tid :: Exp Word32
tid = ThreadIdx X

genType _ Int = "int "
genType _ Int8 = "int8_t "
genType _ Int16 = "int16_t "
genType _ Int32 = "int32_t "
genType _ Int64 = "int64_t "
genType _ Float = "float "
genType _ Double = "double "
genType _ Bool = "int " 
genType _ Word8 = "uint8_t "
genType _ Word16 = "uint16_t "
genType _ Word32 = "uint32_t "
genType _ Word64 = "uint64_t "

genType gc (Pointer t) = genType gc t ++ "*"
genType gc (Global t) = global gc ++" "++ genType gc t  -- "__global " ++ genType t
genType gc (Local t)  = local gc  ++" "++ genType gc t 

genCast gc t = "(" ++ genType gc t ++ ")"

parens s = '(' : s ++ ")"
 
---------------------------------------------------------------------------
-- genExp C-style 
genExp :: Scalar a => GenConfig -> MemMap -> Exp a -> [String]

-- Cheat and do CUDA printing here as well
genExp gc _ (BlockDim X) = ["blockDim.x"]
genExp gc _ (BlockIdx X) = ["blockIdx.x"]
genExp gc _ (BlockIdx Y) = ["blockIdx.y"]
genExp gc _ (BlockIdx Z) = ["blockIdx.z"]
genExp gc _ (ThreadIdx X) = ["threadIdx.x"]
genExp gc _ (ThreadIdx Y) = ["threadIdx.y"]
genExp gc _ (ThreadIdx Z) = ["threadIdx.z"]


genExp gc _ (Literal a) = [show a] 
genExp gc _ (Index (name,[])) = [name]
genExp gc mm exp@(Index (name,es)) = 
  [name' ++ genIndices gc mm es]
  where 
    (offs,t)  = 
      case Map.lookup name mm of  
        Nothing -> error "array does not excist in map" 
        (Just x) -> x
    name' = if mappedName name 
            then parens$ genCast gc t ++ 
                 if offs > 0 
                 then "(sbase+" ++ show offs ++ ")"             
                 else "sbase"
            else name

   
genExp gc mm (BinOp op e1 e2) = 
  [genOp op (genExp gc mm e1 ++ genExp gc mm e2)]

genExp gc mm (UnOp op e) = 
  [genOp op (genExp gc mm e)] 
  
genExp gc mm (If b e1 e2) =   
  [genIf (genExp gc mm b ++ 
          genExp gc mm e1 ++ 
          genExp gc mm e2 )] 

---------------------------------------------------------------------------
--
genIndices gc mm es = concatMap (pIndex mm) es  
  where 
    pIndex mm e = "[" ++ concat (genExp gc mm e) ++ "]"


genIf         [b,e1,e2] = "(" ++ b ++ " ? " ++ e1 ++ " : " ++ e2 ++ ")"

---------------------------------------------------------------------------
-- genOp
genOp :: Op a -> [String] -> String
genOp Add     [a,b] = oper "+" a b 
genOp Sub     [a,b] = oper "-" a b 
genOp Mul     [a,b] = oper "*" a b 
genOp Div     [a,b] = oper "/" a b 

genOp Mod     [a,b] = oper "%" a b 

genOp Sin     [a]   = func "sin" a 
genOp Cos     [a]   = func "cos" a 
-- Bool ops
genOp Eq      [a,b] = oper "==" a b 
genOp Lt      [a,b] = oper "<" a b 
genOp LEq     [a,b] = oper "<=" a b 
genOp Gt      [a,b] = oper ">" a b
genOp GEq     [a,b] = oper ">=" a b

-- Bitwise ops
genOp BitwiseAnd [a,b] = oper "&" a b 
genOp BitwiseOr  [a,b] = oper "|" a b 
genOp BitwiseXor [a,b] = oper "^" a b 
genOp BitwiseNeg [a]   = unOp "~" a 
genOp ShiftL     [a,b] = oper "<<" a b 
genOp ShiftR     [a,b] = oper ">>" a b 


-- built-ins 
genOp Min      [a,b] = func "min" (a ++ "," ++ b) 
genOp Max      [a,b] = func "max" (a ++ "," ++ b) 

genOp Int32ToWord32 [a]  = func "(uint32_t)" a
genOp Word32ToInt32 [a]  = func "(int32_t)" a 

func  f a = f ++ "(" ++ a ++ ")" 
oper  f a b = "(" ++ a ++ f ++ b ++ ")" 
unOp  f a   = "(" ++ f ++ a ++ ")"

---------------------------------------------------------------------------
-- Configurations, threads,memorymap 

data Config = Config {configThreads  :: NumThreads, 
                      configMM       :: MemMap,
                      configLocalMem :: Word32} 
config = Config


assign :: Scalar a => GenConfig -> MemMap -> Exp a -> Exp a -> PP () 
assign gc mm name val = line ((concat (genExp gc mm name)) ++ 
                           " = " ++  concat (genExp gc mm val) ++ 
                           ";") 
                                                    
cond :: GenConfig -> MemMap -> Exp Bool -> PP ()  
cond gc mm e = line ("if " ++ concat (genExp gc mm e))  



-- used in both OpenCL and CUDA generation
potentialCond gc mm n nt pp
  | n < nt = 
    do
      cond gc mm (tid <* (fromIntegral n))
      begin
      pp       
      end 
  | n == nt = pp
              
  | otherwise = error "potentialCond: should not happen"



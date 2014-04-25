{-# LANGUAGE GADTs, 
             TypeFamilies,           
             FlexibleContexts,
             FlexibleInstances, 
             UndecidableInstances,
             OverlappingInstances,
             RankNTypes #-} 

{- Joel Svensson 2012 -} 

module Obsidian.Exp 
       (module Obsidian.Exp,
        module Obsidian.DimSpec) where 



import Data.List
import Data.Word
import Data.Int
import Data.Bits

import qualified Foreign.Storable as Storable

import Obsidian.DimSpec

---------------------------------------------------------------------------
-- Obsidian imports
import Obsidian.Types
import Obsidian.Globs

---------------------------------------------------------------------------
-- some synonyms
--type Data a = Exp a 


type EInt    = Exp Int      
type EWord   = Exp Word

type EInt8   = Exp Int8
type EInt16  = Exp Int16
type EInt32  = Exp Int32
type EInt64  = Exp Int64

type EWord8  = Exp Word8   
type EWord16 = Exp Word16 
type EWord32  = Exp Word32 
type EWord64  = Exp Word64 

type EI8   = Exp Int8
type EI16  = Exp Int16
type EI32  = Exp Int32
type EI64  = Exp Int64

type EW8   = Exp Word8   
type EW16  = Exp Word16 
type EW32  = Exp Word32 
type EW64  = Exp Word64 


type EFloat  = Exp Float  
type EDouble = Exp Double 
type EBool   = Exp Bool    



---------------------------------------------------------------------------
-- Class Scalar. All the things we can handle code generation for 

class (Eq a, ExpToIExp a, Show a) => Scalar a where 
  sizeOf :: Exp a -> Int   --  
  typeOf :: Exp a -> Type  --   Good enough for me ... 


instance Scalar Bool where  
  sizeOf _ = Storable.sizeOf (undefined :: Int)
  typeOf _ = Bool 

instance Scalar Int where 
  sizeOf _ = Storable.sizeOf (undefined :: Int)
  typeOf _ = Int

instance Scalar Int8 where 
  sizeOf _ = 1
  typeOf _ = Int8

instance Scalar Int16 where 
  sizeOf _ = 2
  typeOf _ = Int16

instance Scalar Int32 where 
  sizeOf _ = 4
  typeOf _ = Int32

instance Scalar Int64 where 
  sizeOf _ = 8 
  typeOf _ = Int64
  
instance Scalar Float where
  sizeOf _ = Storable.sizeOf (undefined :: Float)
  typeOf _ = Float
  
instance Scalar Double where 
  sizeOf _ = 8 -- Storable.sizeOf (undefined :: Double) 
  typeOf _ = Double

instance Scalar Word where
  sizeOf _ = Storable.sizeOf (undefined :: Word) 
  typeOf _ = Word
  
instance Scalar Word8 where
  sizeOf _ = 1
  typeOf _ = Word8 

instance Scalar Word16 where 
  sizeOf _ = 2
  typeOf _ = Word16
  
instance Scalar Word32 where 
  sizeOf _ = 4 
  typeOf _ = Word32
  
instance Scalar Word64 where 
  sizeOf _ = 8 
  typeOf _ = Word64



---------------------------------------------------------------------------
-- Expressions 
data Exp a where
  Literal :: Scalar a 
             => a 
             -> Exp a 
  
  {- 
  Add more specific constructors for block,thread variables
   (these concepts excist in both OpenCL and CUDA 
    but are accessed differently so it could be a good 
    idea to add them as constructors here. These 
    can be translated into the CUDA/OpenCL specific 
    concept later in the codegeneration 
  -}
  WarpSize :: Exp Word32
  
  BlockDim :: DimSpec -> Exp Word32
  
  BlockIdx :: DimSpec 
              -> Exp Word32
  ThreadIdx :: DimSpec
               -> Exp Word32
    
  Index   :: Scalar a => 
             (Name,[Exp Word32]) 
             -> Exp a 
             
  If      :: Scalar a 
             => Exp Bool
             -> Exp a 
             -> Exp a 
             -> Exp a 
                          
  BinOp   :: (Scalar a,
              Scalar b, 
              Scalar c) 
             => Op ((a,b) -> c) 
             -> Exp a 
             -> Exp b 
             -> Exp c 
             
  UnOp    :: (Scalar a, 
              Scalar b)
             => Op (a -> b)            
             -> Exp a 
             -> Exp b 
             

  
---------------------------------------------------------------------------
-- Operations 
-- TODO: needs conversion operations.. (Int -> Word) etc. 
data Op a where 
  Add :: Num a => Op ((a,a) -> a) 
  Sub :: Num a => Op ((a,a) -> a) 
  Mul :: Num a => Op ((a,a) -> a) 
  Div :: Num a => Op ((a,a) -> a) 
  
  Mod :: Integral a => Op ((a,a) -> a)
         
  -- Trig
  Sin :: Floating a => Op (a -> a) 
  Cos :: Floating a => Op (a -> a)
  
  -- Comparisons
  Eq  :: Ord a => Op ((a,a) -> Bool)
  NotEq :: Ord a => Op ((a,a) -> Bool) 
  Lt  :: Ord a => Op ((a,a) -> Bool) 
  LEq :: Ord a => Op ((a,a) -> Bool) 
  Gt  :: Ord a => Op ((a,a) -> Bool) 
  GEq :: Ord a => Op ((a,a) -> Bool) 
  
  -- Boolean 
  And :: Op ((Bool,Bool) -> Bool) 
  Or  :: Op ((Bool,Bool) -> Bool)

  Not :: Op (Bool -> Bool) 
  
  -- Bitwise 
  BitwiseAnd :: Bits a => Op ((a,a) -> a) 
  BitwiseOr  :: Bits a => Op ((a,a) -> a)
  BitwiseXor :: Bits a => Op ((a,a) -> a) 
  BitwiseNeg :: Bits a => Op (a -> a)

  -- I DO NOT EVEN KNOW WHAT THIS MEANS: work around it! 
  ShiftL     :: forall a b. (Num b, Bits a) => Op ((a, b) -> a)  
  ShiftR     :: forall a b. (Num b, Bits a) => Op ((a, b) -> a)  
  
  -- built-ins
  Min        :: Ord a => Op ((a,a) -> a) 
  Max        :: Ord a => Op ((a,a) -> a) 

  -- Floating (different CUDA functions for float and double, issue maybe?) 
  Exp :: Floating a => Op (a -> a) -- "expf" 
  Sqrt :: Floating a => Op (a -> a) -- "sqrtf" 
  --RSqrt :: Floating a => Op (a -> a) -- "rsqrtf"
  Log :: Floating a => Op (a -> a) -- "logf"
  Log2 :: Floating a => Op (a -> a) -- "log2f"
  Log10 :: Floating a => Op (a -> a) -- "log10f"
  Pow :: Floating a => Op ((a, a) -> a) -- "powf"
  -- Floating Trig
  Tan :: Floating a => Op (a -> a) -- "tanf"
  ASin :: Floating a => Op (a -> a) -- "asinf"
  ATan :: Floating a => Op (a -> a) -- "atanf"
  ACos :: Floating a => Op (a -> a) -- "acosf"
  SinH :: Floating a => Op (a -> a) -- "sinhf"
  TanH :: Floating a => Op (a -> a) -- "tanhf"
  CosH :: Floating a => Op (a -> a) -- "coshf"
  ASinH :: Floating a => Op (a -> a) -- "asinhf" 
  ATanH :: Floating a => Op (a -> a) -- "atanhf"
  ACosH :: Floating a => Op (a -> a) -- "acoshf"
  -- There is no "div" in "Num" but it's already defined above. 
  FDiv :: Floating a => Op ((a, a) -> a) 

  Int32ToWord32 :: Op (Int32 -> Word32)
  Word32ToInt32 :: Op (Word32 -> Int32)
  Word32ToFloat :: Op (Word32 -> Float)
  Word32ToWord8 :: Op (Word32 -> Word8) 
  

---------------------------------------------------------------------------
-- helpers 

variable name = Index (name,[])
--index name ix = Index (name,[ix])


warpSize :: Exp Word32
warpSize = WarpSize

---------------------------------------------------------------------------
-- Typecasts
---------------------------------------------------------------------------
i32ToW32 = UnOp Int32ToWord32
w32ToI32 = UnOp Word32ToInt32

w32ToF = UnOp Word32ToFloat

w32ToW8 = UnOp Word32ToWord8




---------------------------------------------------------------------------
-- 
instance Scalar a => Show (Exp a) where 
  show = printExp 

-- Look this over. Do I really need a typed expression data type ?
--  (No real need for a Exp GADT I think. Go back to keeping it simple!) 
instance (Eq a, Scalar a) => Eq (Exp a) where
  (==) a b = 
    expToIExp a == expToIExp b
    -- Maybe not efficient! But simple.

  
instance (Scalar a, Ord a) => Ord (Exp a) where 
    min a b = BinOp Min a b
    max a b = BinOp Max a b

---------------------------------------------------------------------------
-- Num instance Exp a?
---------------------------------------------------------------------------
instance (Scalar a ,Num a) => Num (Exp a) where 
  (+) a (Literal 0) = a
  (+) (Literal 0) a = a
  (+) (Literal a) (Literal b) = Literal (a+b)

  -- Added 15 Jan 2013
  (+) (BinOp Mul (BinOp Div x (Literal a)) (Literal b))
       (BinOp Mod y (Literal c))
        | x == y && a == b && b == c = x 
      -- This spots the kind of indexing that occurs from 
      --  converting a bix tix view to and from gix view
        
  -- Added 2 oct 2012
  (+) (BinOp Sub b (Literal a)) (Literal c) | a == c  = b 
  (+) (Literal b) (BinOp Sub a (Literal c)) | b == c  = a 
 
  (+) a b = BinOp Add a b  
  
  (-) a (Literal 0) = a 
  (-) (Literal a) (Literal b) = Literal (a - b) 
  (-) a b = BinOp Sub a b 
  
  (*) a (Literal 1) = a 
  (*) (Literal 1) a = a
  (*) _ (Literal 0) = Literal 0
  (*) (Literal 0) _ = Literal 0
  (*) (Literal a) (Literal b) = Literal (a*b) 
  (*) a b = BinOp Mul a b 
  
  signum = error "signum: not implemented for Exp a"
  abs = error "abs: not implemented for Exp a" 
  fromInteger a = Literal (fromInteger a) 
   
instance (Scalar a, Real a) => Real (Exp a) where 
  toRational = error "toRational: not implemented for Exp a" 
  

instance (Scalar a, Enum a) => Enum (Exp a) where
  toEnum = error "toEnum: not implemented for Exp a" 
  fromEnum = error "fromEnum: not implemented for Exp a" 

instance (Scalar a, Integral a) => Integral (Exp a) where
  mod (Literal a) (Literal b) = Literal (a `mod` b) 
  mod a b = BinOp Mod a b
  div _ (Literal 0) = error "Division by zero in expression" 
  div a b = BinOp Div a b
  quotRem   = error "quotRem: not implemented for Exp a" 
  toInteger = error "toInteger: not implemented for Exp a"
 
---------------------------------------------------------------------------
-- INT Instances
---------------------------------------------------------------------------
-- instance Num (Exp Int) where 
--   (+) a (Literal 0) = a
--   (+) (Literal 0) a = a
--   (+) (Literal a) (Literal b) = Literal (a+b)
--   -- Added 2 Oct 2012
--   (+) (BinOp Sub b (Literal a)) (Literal c) | a == c  = b 
--   (+) (Literal b) (BinOp Sub a (Literal c)) | b == c  = a 
--   (+) a b = BinOp Add a b  
  
--   (-) a (Literal 0) = a 
--   (-) (Literal a) (Literal b) = Literal (a - b) 
--   (-) a b = BinOp Sub a b 
  
--   (*) a (Literal 1) = a 
--   (*) (Literal 1) a = a
--   (*) _ (Literal 0) = Literal 0
--   (*) (Literal 0) _ = Literal 0
--   (*) (Literal a) (Literal b) = Literal (a*b) 
--   (*) a b = BinOp Mul a b 
  
--   signum = error "signum: not implemented for Exp Int" 
--   abs = error "abs: not implemented for Exp Int" 
--   fromInteger a = Literal (fromInteger a) 
  
-- Added new cases for literal 0 (2012/09/25)
instance Bits (Exp Int) where  
  (.&.) x (Literal 0) = Literal 0
  (.&.) (Literal 0) x = Literal 0 
  (.&.) (Literal a) (Literal b) = Literal (a .&. b) 
  (.&.) a b = BinOp BitwiseAnd a b
  (.|.) (Literal a) (Literal b) = Literal (a .|. b)
  (.|.) a b = BinOp BitwiseOr  a b
  xor (Literal a) (Literal b) = Literal (a `xor` b) 
  xor   a b = BinOp BitwiseXor a b 
  
  --TODO: See that this is not breaking something (32/64 bit, CUDA/Haskell)
  complement (Literal i) = Literal (complement i)
  
  complement a = UnOp BitwiseNeg a
  shiftL a i = BinOp ShiftL  a (Literal i)
  shiftR a i = BinOp ShiftR  a (Literal i)
  bitSize a  = sizeOf a * 8
  isSigned a = True

  bit  = error "bit: is undefined for Exp Int"
  testBit = error "testBit: is undefined for Exp Int"
  popCount = error "popCoint: is undefined for Exp Int"


-- TODO: change undefined to some specific error.
-- instance Real (Exp Int) where
--   toRational = error "toRational: not implemented for Exp Int)"  

-- instance Enum (Exp Int) where
--   toEnum = error "toEnum: not implemented for Exp Int" 
--   fromEnum = error "fromEnum: not implemented for Exp Int"
         
-- instance Integral (Exp Int) where
--   mod (Literal a) (Literal b) = Literal (a `mod` b) 
--   mod a b = BinOp Mod a b
--   div _ (Literal 0) = error "Division by zero in expression" 
--   div a b = BinOp Div a b
--   quotRem = error "quotRem: not implemented for Exp Int" 
--   toInteger = error "toInteger: not implemented for Exp Int" 


---------------------------------------------------------------------------
-- Int32
---------------------------------------------------------------------------
-- instance Num (Exp Int32) where 
--   (+) a (Literal 0) = a
--   (+) (Literal 0) a = a
--   (+) (Literal a) (Literal b) = Literal (a+b)
--   -- Added 2 Oct 2012
--   (+) (BinOp Sub b (Literal a)) (Literal c) | a == c  = b 
--   (+) (Literal b) (BinOp Sub a (Literal c)) | b == c  = a 
--   (+) a b = BinOp Add a b  
  
--   (-) a (Literal 0) = a 
--   (-) (Literal a) (Literal b) = Literal (a - b) 
--   (-) a b = BinOp Sub a b 
  
--   (*) a (Literal 1) = a 
--   (*) (Literal 1) a = a
--   (*) _ (Literal 0) = 0
--   (*) (Literal 0) _ = 0 
--   (*) (Literal a) (Literal b) = Literal (a*b) 
--   (*) a b = BinOp Mul a b 
  
--   signum = error "signum: not implemented for Exp Int32"
--   abs = error "abs: not implemented for Exp Int32" 
--   fromInteger a = Literal (fromInteger a) 
  
-- Added new cases for literal 0 (2012/09/25)
instance Bits (Exp Int32) where  
  (.&.) x (Literal 0) = Literal 0
  (.&.) (Literal 0) x = Literal 0 
  (.&.) (Literal a) (Literal b) = Literal (a .&. b) 
  (.&.) a b = BinOp BitwiseAnd a b
  (.|.) (Literal a) (Literal b) = Literal (a .|. b)
  (.|.) a b = BinOp BitwiseOr  a b
  xor (Literal a) (Literal b) = Literal (a `xor` b) 
  xor   a b = BinOp BitwiseXor a b 
  
  --TODO: See that this is not breaking something (32/64 bit, CUDA/Haskell)
  complement (Literal i) = Literal (complement i)
  
  complement a = UnOp BitwiseNeg a
  shiftL a i = BinOp ShiftL  a (Literal i)
  shiftR a i = BinOp ShiftR  a (Literal i)
  bitSize a  = 32 -- sizeeOf a * 8
  isSigned a = True

  bit  = error "bit: is undefined for Exp Int32"
  testBit = error "testBit: is undefined for Exp Int32"
  popCount = error "popCoint: is undefined for Exp Int32"


-- TODO: change undefined to some specific error.
-- instance Real (Exp Int32) where
--   toRational = error "toRational: not implemented for Exp Int32"

-- instance Enum (Exp Int32) where
--   toEnum = error "toEnum: not implemented for Exp Int32" 
--   fromEnum = error "fromEnum: not implemented for Exp Int32" 
         
-- instance Integral (Exp Int32) where
--   mod (Literal a) (Literal b) = Literal (a `mod` b) 
--   mod a b = BinOp Mod a b
--   div _ (Literal 0) = error "Division by zero in expression" 
--   div a b = BinOp Div a b
--   quotRem = error "quotRem: not implemented for Exp Int32" 
--   toInteger = error "toInteger: not implemented for Exp Int32" 


---------------------------------------------------------------------------
-- Word32 Instances
---------------------------------------------------------------------------
-- instance Num (Exp Word32) where 
--   (+) a (Literal 0) = a
--   (+) (Literal 0) a = a
--   (+) (Literal a) (Literal b) = Literal (a+b)

--   -- Added 15 Jan 2013
--   (+) (BinOp Mul (BinOp Div x (Literal a)) (Literal b))
--        (BinOp Mod y (Literal c))
--         | x == y && a == b && b == c = x 
--       -- This spots the kind of indexing that occurs from 
--       --  converting a bix tix view to and from gix view
        
--   -- Added 2 oct 2012
--   (+) (BinOp Sub b (Literal a)) (Literal c) | a == c  = b 
--   (+) (Literal b) (BinOp Sub a (Literal c)) | b == c  = a 
 
--   (+) a b = BinOp Add a b  
  
--   (-) a (Literal 0) = a 
--   (-) (Literal a) (Literal b) = Literal (a - b) 
--   (-) a b = BinOp Sub a b 
  
--   (*) a (Literal 1) = a 
--   (*) (Literal 1) a = a
--   (*) _ (Literal 0) = Literal 0
--   (*) (Literal 0) _ = Literal 0
--   (*) (Literal a) (Literal b) = Literal (a*b) 
--   (*) a b = BinOp Mul a b 
  
--   signum = error "signum: not implemented for Exp Word32"
--   abs = error "abs: not implemented for Exp Word32" 
--   fromInteger a = Literal (fromInteger a) 
  

-- adding special shift operators for when both inputs are 
-- runtime values (2013-01-08) 
(<<*) :: (Scalar b, Scalar a, Bits a, Num b ) => Exp a -> Exp b -> Exp a 
(<<*) a b = BinOp ShiftL a b 

(>>*) :: (Scalar b, Scalar a, Bits a, Num b ) => Exp a -> Exp b -> Exp a 
(>>*) a b = BinOp ShiftR a b 


 -- Added new cases for literal 0 (2012/09/25)
instance Bits (Exp Word32) where 
  (.&.) x (Literal 0) = Literal 0
  (.&.) (Literal 0) x = Literal 0 
  (.&.) (Literal a) (Literal b) = Literal (a .&. b) 
  (.&.) a b = BinOp BitwiseAnd a b   
  (.|.) (Literal a) (Literal b) = Literal (a .|. b) 
  (.|.) a b = BinOp BitwiseOr  a b
  xor (Literal a) (Literal b) = Literal (a `xor` b) 
  xor   a b = BinOp BitwiseXor a b 
  complement (Literal i) = Literal (complement i) 
  complement a = UnOp BitwiseNeg a
  
  shiftL (Literal j) i = Literal (j `shiftL` i) 
  shiftL a i = BinOp ShiftL a (Literal i)
  
  shiftR (Literal j) i = Literal (j `shiftL` i)
  shiftR a i = BinOp ShiftR a (Literal i)
  bitSize a  = 32
  isSigned a = False

  bit  = error "bit: is undefined for Exp Word32"
  testBit = error "testBit: is undefined for Exp Word32"
  popCount = error "popCoint: is undefined for Exp Word32"

-- instance Real (Exp Word32) where 
--   toRational = error "toRational: not implemented for Exp Word32" 
  

-- instance Enum (Exp Word32) where
--   toEnum = error "toEnum: not implemented for Exp Word32" 
--   fromEnum = error "fromEnum: not implemented for Exp Word32" 

-- instance Integral (Exp Word32) where
--   mod (Literal a) (Literal b) = Literal (a `mod` b) 
--   mod a b = BinOp Mod a b
--   div _ (Literal 0) = error "Division by zero in expression"
--   div (Literal a) (Literal b) = Literal (a `div` b) 
--   div a b = BinOp Div a b
--   quotRem = error "quotRem: not implemented for Exp Word32" 
--   toInteger = error "toInteger: not implemented for Exp Word32"
  
-- instance Num (Exp Float) where
--   (+) a (Literal 0) = a
--   (+) (Literal 0) a = a
--   (+) (Literal a) (Literal b) = Literal (a + b)
--   (+) a b = BinOp Add a b
  
--   (-) a (Literal 0) = a
--   (-) (Literal a) (Literal b) = Literal (a - b)
--   (-) a b = BinOp Sub a b
  
--   (*) a (Literal 1) = a
--   (*) (Literal 1) a = a
--   (*) _ (Literal 0) = Literal 0
--   (*) (Literal 0) _ = Literal 0
--   (*) (Literal a) (Literal b) = Literal (a * b)
--   (*) a b = BinOp Mul a b
  
--   signum = undefined
--   abs = undefined
--   fromInteger a = Literal (fromInteger a)

instance Fractional (Exp Float) where
  (/) (Literal a) (Literal b) = Literal (a/b)
  (/) a b = BinOp FDiv a b
  recip a = (Literal 1) / a
  fromRational a = Literal (fromRational a)

instance Floating (Exp Float) where
  pi = Literal pi
  exp a = UnOp Exp a
  sqrt a = UnOp Sqrt a
  log a = UnOp Log a
  (**) a b = BinOp Pow a b
  
  -- log_b(x) = log_e(x) / log_e(b)
  logBase (Literal 2) b = UnOp Log2 b
  logBase (Literal 10) b = UnOp Log10 b
  logBase a b = (UnOp Log b) / (UnOp Log a)
  
  sin (Literal 0) = Literal 0
  sin a = UnOp Sin a
  tan (Literal 0) = Literal 0
  tan a = UnOp Tan a
  cos (Literal 0) = Literal 1
  cos a = UnOp Cos a
  
  asin (Literal 0) = Literal 0
  asin a = UnOp ASin a
  atan (Literal 0) = Literal 0
  atan a = UnOp ATan a
  acos (Literal 1) = Literal 0
  acos a = UnOp ACos a
  
  sinh (Literal 0) = Literal 0
  sinh a = UnOp Sin a
  tanh (Literal 0) = Literal 0
  tanh a = UnOp Tan a
  cosh (Literal 0) = Literal 1
  cosh a = UnOp Cos a
  
  asinh a = UnOp ASinH a
  atanh a = UnOp ATanH a
  acosh a = UnOp ACosH a
  
  -- Y-Less's comment
  -- Don't second guess the CUDA compiler (or, more accurately, assume that
  -- other compilers have this).
  --(/) (Literal 1) (UnOp Sqrt b) = UnOp RSqrt b -- Optimisation.

---------------------------------------------------------------------------
  
infix 4 ==*, /=*, <*, >*, >=*, <=* 
  
(==*) (Literal a) (Literal b) = Literal (a == b) 
(==*) a b = BinOp Eq a b
(/=*) a b = BinOp NotEq a b 
(<*)  (Literal a) (Literal b) = Literal (a < b) 
(<*)  a b = BinOp Lt a b
(<=*) (Literal a) (Literal b) = Literal (a <= b) 
(<=*) a b = BinOp LEq a b
(>*)  a b = BinOp Gt  a b
(>=*) a b = BinOp GEq a b

infixr 3 &&*
infixr 2 ||* 
(&&*) a b = BinOp And a b 
(||*) a b = BinOp Or a b 

notE = UnOp Not
---------------------------------------------------------------------------
-- Choice class
---------------------------------------------------------------------------
class Choice a where 
  ifThenElse :: Exp Bool -> a -> a -> a 

instance Scalar a => Choice (Exp a) where  
  ifThenElse (Literal False) e1 e2 = e2
  ifThenElse (Literal True)  e1 e2 = e1
  ifThenElse b e1 e2 = If b e1 e2
  
instance (Choice a, Choice b) => Choice (a,b) where
  ifThenElse b (e1,e1') (e2,e2') = (ifThenElse b e1 e2,
                                    ifThenElse b e1' e2')

instance (Choice a, Choice b, Choice c) => Choice (a,b,c) where
  ifThenElse b (e1,e1',e1'') (e2,e2',e2'') = (ifThenElse b e1 e2,
                                              ifThenElse b e1' e2',
                                              ifThenElse b e1'' e2'')

  
---------------------------------------------------------------------------
-- Print Expressions
---------------------------------------------------------------------------
  
printExp :: Scalar a => Exp a -> String
printExp (BlockIdx X) = "blockIdx.x"
printExp (ThreadIdx X) = "threadIdx.x"
printExp (BlockDim X)   = "blockDim.x"
printExp (Literal a) = show a 
printExp (Index (name,[])) = name
printExp (Index (name,es)) = 
  name ++ "[" ++ ((concat . intersperse "," . map printExp) es) ++ "]"
printExp (BinOp op e1 e2) = "(" ++ printOp op ++ " " ++  printExp e1 ++ " " ++ printExp e2 ++ " )"
printExp (UnOp  op e) = "(" ++ printOp op ++ " " ++ printExp e ++ " )"
printExp (If b e1 e2) = "(" ++ printExp b ++ " ? " ++ printExp e1 ++ " : " ++ printExp e2 ++ ")"


printOp :: Op a -> String
printOp Add = " + " 
printOp Sub = " - " 
printOp Mul = " * "
printOp Div = " / "
printOp Mod = " % "

-- printOp If  = " if "

printOp Eq  = " == "
printOp NotEq = " /= " 
printOp Lt  = " < " 
printOp LEq = " <= " 
printOp Gt  = " > "
printOp GEq = " >= " 

printOp And = " && "
printOp Or  = " || "

printOp Min = " Min "
printOp Max = " Max " 

printOp Sin = " Sin " 
printOp Cos = " Cos "

printOp BitwiseAnd = " & "
printOp BitwiseOr  = " | " 
printOp BitwiseXor = " ^ " 
printOp BitwiseNeg = " ~ "  


---------------------------------------------------------------------------
-- Internal exp (not a GADT) 
---------------------------------------------------------------------------

data IExp = IVar Name Type
          | IBlockIdx  DimSpec
          | IThreadIdx DimSpec
          | IBlockDim  DimSpec
          | IGridDim   DimSpec

          | IBool Bool 
          | IInt8 Int8 | IInt16 Int16 | IInt32 Int32 | IInt64 Int64
          | IWord8 Word8 | IWord16 Word16 | IWord32 Word32 | IWord64 Word64
          | IFloat Float | IDouble Double
                           
          | IIndex (IExp,[IExp]) Type
          | ICond IExp IExp IExp Type
          | IBinOp IBinOp IExp IExp Type
          | IUnOp  IUnOp  IExp Type
          | IFunCall Name [IExp] Type
          | ICast IExp Type
          deriving (Eq, Ord, Show) 
 

data IBinOp = IAdd | ISub | IMul | IDiv | IMod
            | IEq | INotEq | ILt | IGt | IGEq | ILEq
            | IAnd | IOr | IPow
            | IBitwiseAnd | IBitwiseOr | IBitwiseXor
            | IShiftL | IShiftR
            deriving (Eq, Ord, Show) 

data IUnOp = IBitwiseNeg | INot
           deriving (Eq, Ord, Show)



---------------------------------------------------------------------------
-- Remove type info from operations
---------------------------------------------------------------------------

binOpToIBinOp :: Op t -> IBinOp
binOpToIBinOp Add = IAdd
binOpToIBinOp Sub = ISub
binOpToIBinOp Mul = IMul
binOpToIBinOp Div = IDiv 
binOpToIBinOp FDiv = IDiv -- (???)  
binOpToIBinOp Mod = IMod

binOpToIBinOp Eq  = IEq 
binOpToIBinOp NotEq = INotEq
binOpToIBinOp Lt  = ILt 
binOpToIBinOp LEq = ILEq
binOpToIBinOp Gt  = IGt 
binOpToIBinOp GEq = IGEq 

binOpToIBinOp And = IAnd 
binOpToIBinOp Or  = IOr 

binOpToIBinOp Pow = IPow

binOpToIBinOp BitwiseAnd = IBitwiseAnd
binOpToIBinOp BitwiseOr  = IBitwiseOr
binOpToIBinOp BitwiseXor = IBitwiseXor
binOpToIBinOp ShiftL     = IShiftL 
binOpToIBinOp ShiftR     = IShiftR

unOpToIUnOp :: Op t -> IUnOp
unOpToIUnOp   BitwiseNeg = IBitwiseNeg
unOpToIUnOp   Not = INot 


---------------------------------------------------------------------------
-- Turn Exp a to IExp with type information. 
---------------------------------------------------------------------------


class ExpToIExp a where 
  expToIExp :: Exp a -> IExp 


instance  ExpToIExp Bool where 
  expToIExp (Literal True) = IBool True 
  expToIExp (Literal False) = IBool False 
  expToIExp a = expToIExpGeneral a 

-- This is strange. 
instance ExpToIExp Int where 
  expToIExp (Literal a) = IInt32 (fromIntegral a)  
  expToIExp a = expToIExpGeneral a  

instance ExpToIExp Int8 where 
  expToIExp (Literal a) = IInt8 a 
  expToIExp a = expToIExpGeneral a  

instance ExpToIExp Int16 where 
  expToIExp (Literal a) = IInt16 a 
  expToIExp a = expToIExpGeneral a  

instance ExpToIExp Int32 where 
  expToIExp (Literal a) = IInt32 a 
  expToIExp a = expToIExpGeneral a  

instance ExpToIExp Int64 where 
  expToIExp (Literal a) = IInt64 a 
  expToIExp a = expToIExpGeneral a  

instance ExpToIExp Float where 
  expToIExp (Literal a) = IFloat a 
  expToIExp a = expToIExpGeneral a 

instance ExpToIExp Double where 
  expToIExp (Literal a) = IDouble a 
  expToIExp a = expToIExpGeneral a 

-- This is strange. 
instance ExpToIExp Word where 
  expToIExp (Literal a) = IWord32 (fromIntegral a)
  expToIExp a = expToIExpGeneral a 

instance ExpToIExp Word8 where 
  expToIExp (Literal a) = IWord8 a 
  expToIExp a = expToIExpGeneral a 

instance ExpToIExp Word16 where 
  expToIExp (Literal a) = IWord16 a 
  expToIExp a = expToIExpGeneral a 

instance ExpToIExp Word32 where 
  expToIExp (Literal a) = IWord32 a 
  expToIExp a = expToIExpGeneral a 

instance ExpToIExp Word64 where 
  expToIExp (Literal a) = IWord64 a 
  expToIExp a = expToIExpGeneral a


  
expToIExpGeneral :: ExpToIExp a  => Exp a -> IExp
expToIExpGeneral WarpSize      = IVar "warpsize" Word32 
expToIExpGeneral (BlockIdx d)  = IBlockIdx d
expToIExpGeneral (BlockDim d)  = IBlockDim d 
expToIExpGeneral (ThreadIdx d) = IThreadIdx d

expToIExpGeneral e@(Index (name,[])) = IVar name  (typeOf e)
expToIExpGeneral e@(Index (name,xs))
  = IIndex (IVar name (Pointer (typeOf e)),map expToIExp xs) (typeOf e) 
expToIExpGeneral e@(If b e1 e2)
  = ICond  (expToIExp b) (expToIExp e1) (expToIExp e2) (typeOf e)


expToIExpGeneral (UnOp Word32ToInt32 e) = ICast (expToIExp e) Int32
expToIExpGeneral (UnOp Int32ToWord32 e) = ICast (expToIExp e) Word32
expToIExpGeneral (UnOp Word32ToFloat e) = ICast (expToIExp e) Float
expToIExpGeneral (UnOp Word32ToWord8 e) = ICast (expToIExp e) Word8 

expToIExpGeneral e@(BinOp Min e1 e2)
  = IFunCall "min" [expToIExp e1, expToIExp e2] (typeOf e)
    
expToIExpGeneral e@(BinOp Max e1 e2)
  = IFunCall "max" [expToIExp e1, expToIExp e2] (typeOf e)
    
expToIExpGeneral e@(BinOp op e1 e2)
  = IBinOp (binOpToIBinOp op) (expToIExp e1) (expToIExp e2) (typeOf e)


expToIExpGeneral (UnOp Exp e)        = IFunCall "exp" [expToIExp e] (typeOf e)
expToIExpGeneral (UnOp Sqrt e)       = IFunCall "sqrt" [expToIExp e] (typeOf e)
expToIExpGeneral (UnOp Log e)        = IFunCall "log" [expToIExp e]  (typeOf e)
expToIExpGeneral (UnOp Log2 e)       = IFunCall "log2" [expToIExp e] (typeOf e)
expToIExpGeneral (UnOp Log10 e)      = IFunCall "log10" [expToIExp e] (typeOf e)
  
-- Floating trig
expToIExpGeneral (UnOp Sin e)        = IFunCall "sin" [expToIExp e] (typeOf e)
expToIExpGeneral (UnOp Cos e)        = IFunCall "cos" [expToIExp e] (typeOf e)
expToIExpGeneral (UnOp Tan e)        = IFunCall "tan" [expToIExp e] (typeOf e)
expToIExpGeneral (UnOp ASin e)       = IFunCall "asin" [expToIExp e] (typeOf e)
expToIExpGeneral (UnOp ACos e)       = IFunCall "acos" [expToIExp e] (typeOf e)
expToIExpGeneral (UnOp ATan e)       = IFunCall "atan" [expToIExp e] (typeOf e)
expToIExpGeneral (UnOp SinH e)       = IFunCall "sinh" [expToIExp e] (typeOf e)
expToIExpGeneral (UnOp CosH e)       = IFunCall "cosh" [expToIExp e] (typeOf e)
expToIExpGeneral (UnOp TanH e)       = IFunCall "tanh" [expToIExp e]  (typeOf e)
expToIExpGeneral (UnOp ASinH e)      = IFunCall "asinh" [expToIExp e] (typeOf e)
expToIExpGeneral (UnOp ACosH e)      = IFunCall "acosh" [expToIExp e] (typeOf e)
expToIExpGeneral (UnOp ATanH e)      = IFunCall "atanh" [expToIExp e] (typeOf e)

expToIExpGeneral e@(UnOp op e1) = IUnOp  (unOpToIUnOp op) (expToIExp e1) (typeOf e)
 

---------------------------------------------------------------------------
-- Collect arrays from an IExp 
---------------------------------------------------------------------------

collectArraysI :: String -> IExp -> [Name]
collectArraysI pre e = go e
  where
    go (IVar name _) = if isPrefixOf pre name then [name] else []
    go (IIndex (ne,es) _) = go ne ++ concatMap go es 
    go (IBinOp _ e1 e2 _) = go e1 ++ go e2
    go (IUnOp  _ e _) = go e
    go (ICond b e1 e2 _) = go b ++ go e1 ++ go e2
    go (IFunCall _ es _) = concatMap go es
    go (ICast e _) = go e 
    go _ = [] 



{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-} 

module Obsidian.CodeGen.CompileIM where 

import Language.C.Quote
import Language.C.Quote.CUDA as CU

import qualified Language.C.Quote.OpenCL as CL 

import qualified "language-c-quote" Language.C.Syntax as C 

import Obsidian.Exp (IExp(..),IBinOp(..),IUnOp(..))
import Obsidian.Types
import Obsidian.DimSpec 
import Obsidian.CodeGen.Program



import Data.Word
import Data.Int

{- TODOs:
   * Do not "derive" number of threads from the code.
     Impose a number of threads and blocks from the outside
   * Pass a target "platform" to code generator.
      - CUDA
      - OpenCL
      - Sequential C
   * Create the full kernel (including function declaration, inputs, outputs
   * Move memory-mapping to a earlier IM -> IM phase.
   
-} 

---------------------------------------------------------------------------
-- Platform
---------------------------------------------------------------------------
data Platform = PlatformCUDA
              | PlatformOpenCL
              | PlatformC 

data Config = Config {configThreads :: Word32,
                      configBlocks  :: Word32 }



---------------------------------------------------------------------------
-- compileExp (maybe a bad name)
---------------------------------------------------------------------------
compileExp :: IExp -> Exp 
compileExp (IVar name t) = [cexp| $id:name |]

compileExp (IBlockIdx X) = [cexp| $id:("blockIdx.x") |]
compileExp (IBlockIdx Y) = [cexp| $id:("blockIdx.y") |]
compileExp (IBlockIdx Z) = [cexp| $id:("blockIdx.z") |]

compileExp (IThreadIdx X) = [cexp| $id:("threadIdx.x") |]
compileExp (IThreadIdx Y) = [cexp| $id:("threadIdx.y") |]
compileExp (IThreadIdx Z) = [cexp| $id:("threadIdx.z") |]

compileExp (IBlockDim X) = [cexp| $id:("blockDim.x") |]
compileExp (IBlockDim Y) = [cexp| $id:("blockDim.y") |]
compileExp (IBlockDim Z) = [cexp| $id:("blockDim.z") |]

compileExp (IGridDim X) = [cexp| $id:("GridDim.x") |]
compileExp (IGridDim Y) = [cexp| $id:("GridDim.y") |]
compileExp (IGridDim Z) = [cexp| $id:("GridDim.z") |]

compileExp (IBool True) = [cexp|1|]
compileExp (IBool False) = [cexp|0|]
compileExp (IInt8 n) = [cexp| $int:(toInteger n) |]
compileExp (IInt16 n) = [cexp| $int:(toInteger n) |]
compileExp (IInt32 n) = [cexp| $int:(toInteger n) |]
compileExp (IInt64 n) = [cexp| $lint:(toInteger n) |]

compileExp (IWord8 n) = [cexp| $uint:(toInteger n) |]
compileExp (IWord16 n) = [cexp| $uint:(toInteger n) |]
compileExp (IWord32 n) = [cexp| $uint:(toInteger n) |]
compileExp (IWord64 n) = [cexp| $ulint:(toInteger n) |]

compileExp (IFloat n) = [cexp| $float:(toRational n) |]
compileExp (IDouble n) = [cexp| $double:(toRational n) |]

compileExp (IIndex (i1,[e]) t) = [cexp| $(compileExp i1)[$(compileExp e)] |] 

compileExp (ICond e1 e2 e3 t) = [cexp| $(compileExp e1) ? $(compileExp e2) : $(compileExp e3) |]

compileExp (IBinOp op e1 e2 t) = go op 
  where
    x = compileExp e1
    y = compileExp e2
    go IAdd = [cexp| $x + $y |]
    go ISub = [cexp| $x - $y |]
    go IMul = [cexp| $x * $y |]
    go IDiv = [cexp| $x / $y |]
    go IMod = [cexp| $x % $y |]
    go IEq = [cexp| $x == $y |]
    go INotEq = [cexp| $x !=  $y |]
    go ILt = [cexp| $x < $y |]
    go IGt = [cexp| $x > $y |]
    go IGEq = [cexp| $x >= $y |]
    go ILEq = [cexp| $x <=  $y |]
    go IAnd = [cexp| $x && $y |]
    go IOr = [cexp| $x  || $y |]
    go IPow = case t of
                Float ->  [cexp|powf($x,$y) |]
                Double -> [cexp|pow($x,$y)  |] 
    go IBitwiseAnd = [cexp| $x & $y |]
    go IBitwiseOr = [cexp| $x | $y |]
    go IBitwiseXor = [cexp| $x ^ $y |]
    go IShiftL = [cexp| $x << $y |]
    go IShiftR = [cexp| $x >> $y |]
compileExp (IUnOp op e t) = go op
  where
    x = compileExp e
    go IBitwiseNeg = [cexp| ~$x|]
    go INot        = [cexp| !$x|]
    
compileExp (IFunCall name es t) = [cexp| $fc |]
  where
    es' = map compileExp es
    fc  = [cexp| $id:(name)($args:(es')) |]

compileExp (ICast e t) = [cexp| ($ty:(go t)) $e' |]
  where
    e' = compileExp e
    go Int8   = [cty| typename int8_t  |]
    go Int16  = [cty| typename int16_t |]
    go Int32  = [cty| typename int32_t |]
    go Int64  = [cty| typename int64_t |]

    go Word8  = [cty| typename uint8_t  |]
    go Word16 = [cty| typename uint16_t |]
    go Word32 = [cty| typename uint32_t |]
    go Word64 = [cty| typename uint64_t |]

    go Float  = [cty| float  |]
    go Double = [cty| double |]

    go (Pointer t) = [cty| $ty:(go t)* |]
 

---------------------------------------------------------------------------
-- **     
-- Compile IM
-- ** 
--------------------------------------------------------------------------- 

---------------------------------------------------------------------------
-- Statement t to Stm
---------------------------------------------------------------------------


compileStm :: Statement t -> [Stm]
compileStm (SAssign name [] e)
  = [[cstm| $(compileExp name) = $(compileExp e);|]]
compileStm (SAssign name [ix] e) 
  = [[cstm| $(compileExp name)[$(compileExp ix)] = $(compileExp e); |]]
compileStm (SCond be im) 
  = [[cstm| if ($(compileExp be)) { $stms:(compileIM  im) } |]]
compileStm (SForAll n im) 
  = [[cstm| if (threadIdx.x < $(compileExp n)) { $stms:(compileIM im) } |]]
compileStm (SForAllBlocks n im) 
  = [[cstm| if (blockIdx.x < $(compileExp n)) { $stms:(compileIM im) } |]]
compileStm SSynchronize 
  = [[cstm| __syncthreads(); |]]
compileStm a = []
         
  
compileIM :: IMList a -> [Stm]
compileIM im = concatMap (compileStm . fst) im


---------------------------------------------------------------------------
-- Generate entire Kernel 
---------------------------------------------------------------------------
type Parameters = [(String,Obsidian.Types.Type)]

compile :: Platform -> Config -> String -> (Parameters,IMList a) -> Definition
compile pform config kname (params,im)
  = go pform 
  where
    stms = compileIM im 
    ps = compileParams params
    go PlatformCUDA
      = [cedecl| extern "C" __global__ void $id:kname($params:ps) {$items:body} |]
    go PlatformOpenCL
      = [CL.cedecl| __kernel void $id:kname($params:ps) {$stms:stms} |]

    body = [BlockDecl cudaDecls] ++
            map BlockStm stms

-- see if there is a problem with alignments
-- Maybe Obsidian.CodeGen.Memory needs to align allocations.. 
cudaDecls = [cdecl| extern __shared__ typename uint8_t sbase[]; |]

compileParams :: Parameters -> [Param]
compileParams = map go
  where
    go (name,t) = [cparam| $ty:(cType t) $id:name |]

    cType (Int32) = [cty| typename int32_t |]
    cType (Word32) = [cty| typename uint32_t |]
    cType (Pointer t) = [cty| $ty:(cType t)* |]

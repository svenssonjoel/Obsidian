
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-} 

{-

   Joel Svensson 2013


-} 

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
    
   * Pass a target "platform" to code generator.
      - CUDA
      - OpenCL
      - Sequential C
      - C with OpenMP ? 
   * rewrite some functions here to use  a reader monad. 
   
-} 

---------------------------------------------------------------------------
-- Platform
---------------------------------------------------------------------------
data Platform = PlatformCUDA
              | PlatformOpenCL
              | PlatformC

data Config = Config { configThreadsPerBlock :: Word32,
                       configSharedMem :: Word32 }




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

compileExp (ICast e t) = [cexp| ($ty:(compileType t)) $e' |]
  where
    e' = compileExp e
   
compileType (Int8) = [cty| typename int8_t |]
compileType (Int16) = [cty| typename int16_t |]
compileType (Int32) = [cty| typename int32_t |]
compileType (Int64) = [cty| typename int64_t |]
compileType (Word8) = [cty| typename uint8_t |]
compileType (Word16) = [cty| typename uint16_t |]
compileType (Word32) = [cty| typename uint32_t |]
compileType (Word64) = [cty| typename uint64_t |]
compileType (Float) = [cty| float |]
compileType (Double) = [cty| double |]
compileType (Pointer t) = [cty| $ty:(compileType t)* |]


---------------------------------------------------------------------------
-- **     
-- Compile IM
-- ** 
--------------------------------------------------------------------------- 

---------------------------------------------------------------------------
-- Statement t to Stm
---------------------------------------------------------------------------


compileStm :: Platform -> Config -> Statement t -> [Stm]
compileStm p c (SAssign name [] e)
  = [[cstm| $(compileExp name) = $(compileExp e);|]]
compileStm p c (SAssign name [ix] e) 
  = [[cstm| $(compileExp name)[$(compileExp ix)] = $(compileExp e); |]]
compileStm p c (SCond be im) 
  = [[cstm| if ($(compileExp be)) { $stms:(compileIM p c im) } |]]
compileStm p c (SSeqFor loopVar n im) 
  = [[cstm| for (int $id:loopVar = 0; $id:loopVar < $(compileExp n); ++$id:loopVar) 
                { $stms:(compileIM p c im) } |]]
compileStm p c a@(SForAll loopVar n im) = compileForAll p c a
--   = [[cstm| if (threadIdx.x < $(compileExp n)) { $stms:(compileIM p c im) } |]]
compileStm p c (SForAllBlocks n im) 
  = [[cstm| if (blockIdx.x < $(compileExp n)) { $stms:(compileIM p c im) } |]]
compileStm p c (SNWarps n im) = compileWarp p c n im 
compileStm p c SSynchronize 
  = case p of
      PlatformCUDA -> [[cstm| __syncthreads(); |]]
      PlatformOpenCL -> [[cstm| barrier(CLK_LOCAL_MEM_FENCE); |]]
compileStm _ _ (SWarpForAll _ _  n im) = error "WarpForAll"
compileStm _ _ a = [] -- error  $ "compileStm: missing case "

---------------------------------------------------------------------------
-- ForAll is compiled differently for different platforms
---------------------------------------------------------------------------
compileForAll PlatformCUDA c (SForAll loopVar (IWord32 n) im) = goQ ++ goR
  where
    nt = configThreadsPerBlock c 

    q  = n `quot` nt
    r  = n `rem`  nt 
    
    goQ  =
      case q of
        0 -> []
        1 -> [cstm| $id:loopVar = threadIdx.x; |] : compileIM PlatformCUDA c im 
        n -> [[cstm| for ( int i = 0; i < $int:q; ++i) { $stms:body } |]]
             where 
               body = [cstm|$id:loopVar =  i*$int:nt + threadIdx.x; |] :compileIM PlatformCUDA c im
   
    goR  = 
      case r of 
        0 -> [] 
        n -> [[cstm| if (threadIdx.x < $int:n) { 
                       $id:loopVar = $int:(q*nt) + threadIdx.x;  
                       $stms:(compileIM PlatformCUDA c im) } |]]
 
compileForAll PlatformC c (SForAll loopVar (IWord32 n) im) = go 
  where
    body = compileIM PlatformC c im    
    go = [ [cstm| for (int i = 0; i <$int:n; ++i) { $stms:body } |] ] 
      

---------------------------------------------------------------------------
-- compileWarp (needs fixing so that warpIx and warpID are always correct) 
---------------------------------------------------------------------------
compileWarp PlatformCUDA c (IWord32 warps) im 
  = concatMap (go . fst)  im
  where
    go (SAllocate nom n t) = []
    go (SWarpForAll warpID warpIx (IWord32 n) im)
      = case (wholeRealWarps `compare` warps) of
          GT ->
            [[cstm| if (threadIdx.x < $int:(warps*32)) {
                      $stms:(goQ ++ goR) } |]]   

          EQ -> goQ ++ goR

          LT -> wQ ++ wR
            
      where
        nt = configThreadsPerBlock c
        wholeRealWarps = nt `quot` 32
        threadsPartialWarp = nt `rem` 32 -- Do not use partial warps! 
        nVirtualWarps = warps - wholeRealWarps

        threadQ = n `quot` 32   -- Set up virtual threads within warp
        threadR = n `rem`  32   --    
        goQ = case threadQ of 
                0 -> [] 
                1 -> [cstm| $id:warpIx = threadIdx.x % 32; |]:compileIM PlatformCUDA c im
                n -> [[cstm| for (int i = 0; i < $int:threadQ; ++i) { $stms:body } |]]
                    where 
                      body = [cstm| $id:warpIx = i*32 + threadIdx.x % 32; |] : compileIM PlatformCUDA c im  
        goR = case threadR of 
                0 -> [] 
                n -> [[cstm| if ( threadIdx.x % 32 < $int:n) { 
                               $id:warpIx = $int:(threadQ*32) + threadIdx.x % 32;
                               $stms:(compileIM PlatformCUDA c im) } |]]

        warpQ = warps `quot` wholeRealWarps 
        warpR = warps `rem`  wholeRealWarps

        wQ = case warpQ of 
               0 -> [] 
               1 -> goQ 
               n -> [[cstm| for (int vw = 0; vw < $int:warpQ; ++vw) { $stms:body } |]]
                   where 
                     body = [cstm| $id:warpID = vw*$int:wholeRealWarps + threadIdx.x / 32; |] : compileIM PlatformCUDA c im 

        wR = case warpR of 
               0 -> [] 
               n -> [[cstm| if (threadIdx.x / 32 < $int:n) {
                              $id:warpID = $int:(warpQ*wholeRealWarps) + threadIdx.x / 32; 
                              $stms:(compileIM PlatformCUDA c im) } |]]
--------------------------------------------------------------------------- 
-- CompileIM to list of Stm 
--------------------------------------------------------------------------- 
compileIM :: Platform -> Config -> IMList a -> [Stm]
compileIM pform conf im = concatMap ((compileStm pform conf) . fst) im


---------------------------------------------------------------------------
-- Generate entire Kernel 
---------------------------------------------------------------------------
type Parameters = [(String,Obsidian.Types.Type)]

compile :: Platform -> Config -> String -> (Parameters,IMList a) -> Definition
compile pform config kname (params,im)
  = go pform 
  where
    stms = compileIM pform config im 
    ps = compileParams pform params
    go PlatformCUDA
      = [cedecl| extern "C" __global__ void $id:kname($params:ps) {$items:cudabody} |]
    go PlatformOpenCL
      = [CL.cedecl| __kernel void $id:kname($params:ps) {$stms:stms} |]
    go PlatformC
      = [cedecl| extern "C" void $id:kname($params:ps) {$items:cbody} |] 

    cudabody = [BlockDecl [cdecl| extern __shared__ typename uint8_t sbase[]; |], 
                BlockDecl [cdecl| typename uint32_t tid = threadIdx.x; |],
                BlockDecl [cdecl| typename uint32_t warpID = threadIdx.x / 32; |],
                BlockDecl [cdecl| typename uint32_t warpIx = threadIdx.x % 32; |] ] ++
                map BlockStm stms

    cbody = -- add memory allocation 
            map BlockStm stms


--------------------------------------------------------------------------- 
-- Parameter lists for functions  (kernel head) 
---------------------------------------------------------------------------
compileParams :: Platform -> Parameters -> [Param]
compileParams PlatformOpenCL = map go
  where
    go (name,Pointer t) = [CL.cparam| global  $ty:(compileType t) $id:name |]
    go (name, t)        = [CL.cparam| $ty:(compileType t) $id:name |]

-- C or CUDA 
compileParams _ = map go
  where
    go (name,t) = [cparam| $ty:(compileType t) $id:name |]
 

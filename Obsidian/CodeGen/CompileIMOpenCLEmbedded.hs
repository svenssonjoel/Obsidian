
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 

{-

   Joel Svensson 2017

   * notes:
      2017-04-23: Initial version. Duplicates a lot of the CUDA generation.

   * Todo:

     - Make sure generated code makes sense in Vivado_HLS

     - See what happens to the Workgroup interface generated
       by Vivado_HLS when using these:
       * get_num_groups()
       * get_local_size()
       * get_group_id()
       * get_local_id()
       * get_global_id()
       * get_global_size()

       get_num_groups and get_global_size are interesting as I have
       not seen these communicated to a Vivado_HLS opencl workgroup.
       It may be that interface is only generated when these functions
       are used. 


-} 

module Obsidian.CodeGen.CompileIMOpenCLEmbedded where

import Language.C.Quote.OpenCL

import qualified "language-c-quote" Language.C.Syntax as C

import Obsidian.Exp (IExp(..),IBinOp(..),IUnOp(..))
import Obsidian.Types as T
import Obsidian.DimSpec 
import Obsidian.CodeGen.Program

import Data.Word

{- Notes:

   * Generation of OpenCL targeted at embedded profile
     as it presents itself in VivadoHLS 

-} 

---------------------------------------------------------------------------
-- Config
---------------------------------------------------------------------------

data Config = Config { configThreadsPerBlock :: Word32,
                       configSharedMem :: Word32}


---------------------------------------------------------------------------
-- Some strings for OpenCLifying the generator 
---------------------------------------------------------------------------
threadIdx_x = [cexp| get_local_id(0) |]
blockIdx_x  = [cexp| get_group_id(0) |]
globalIdx_x = [cexp| get_global_id(0)|]
blockDim_x  = [cexp| get_local_size(0)|]



---------------------------------------------------------------------------
-- compileExp (maybe a bad name)
---------------------------------------------------------------------------
compileExp :: IExp -> C.Exp 
compileExp (IVar name t) = [cexp| $id:name |]


-- TODO: Fix all this! 
-- compileExp (IBlockIdx X) = [cexp| $id:("bid")|] -- [cexp| $id:("blockIdx.x") |]
-- compileExp (IBlockIdx Y) = [cexp| $id:("blockIdx.y") |]
-- compileExp (IBlockIdx Z) = [cexp| $id:("blockIdx.z") |]

-- compileExp (IThreadIdx X) = [cexp| $id:("threadIdx.x") |]
-- compileExp (IThreadIdx Y) = [cexp| $id:("threadIdx.y") |]
-- compileExp (IThreadIdx Z) = [cexp| $id:("threadIdx.z") |]

-- compileExp (IBlockDim X) = [cexp| $id:("blockDim.x") |]
-- compileExp (IBlockDim Y) = [cexp| $id:("blockDim.y") |]
-- compileExp (IBlockDim Z) = [cexp| $id:("blockDim.z") |]

-- compileExp (IGridDim X) = [cexp| $id:("GridDim.x") |]
-- compileExp (IGridDim Y) = [cexp| $id:("GridDim.y") |]
-- compileExp (IGridDim Z) = [cexp| $id:("GridDim.z") |]

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

compileExp (IFloat n) = [cexp| $float:(n) |]
compileExp (IDouble n) = [cexp| $double:(n) |]

-- Implementing these may be a bit awkward
-- given there are no vector literals in cuda. 
compileExp (IFloat2 n m) = error "IFloat2 unhandled"
compileExp (IFloat3 n m l) = error "IFloat3 unhandled"
compileExp (IFloat4 n m l k) = error "IFloat4 unhandled"
compileExp (IDouble2 n m) = error "IDouble2 unhandled" 
compileExp (IInt8_2 n m) = error "FIXME"
compileExp (IInt8_3 n m k) = error "FIXME"
compileExp (IInt8_4 n m k l) = error "FIXME"
compileExp (IInt16_2 n m ) = error "FIXME"
compileExp (IInt16_3 n m k) = error "FIXME"
compileExp (IInt16_4 n m k l) = error "FIXME"
compileExp (IInt32_2 n m) = error "FIXME"
compileExp (IInt32_3 n m k) = error "FIXME"
compileExp (IInt32_4 n m k l) = error "FIXME"
compileExp (IInt64_2 n m) = error "FIXME"
compileExp (IInt64_3 n m k) = error "FIXME"
compileExp (IInt64_4 n m k l) = error "FIXME"
compileExp (IWord8_2 n m) = error "FIXME"
compileExp (IWord8_3 n m k) = error "FIXME"
compileExp (IWord8_4 n m k l) = error "FIXME"
compileExp (IWord16_2 n m ) = error "FIXME"
compileExp (IWord16_3 n m k) = error "FIXME"
compileExp (IWord16_4 n m k l) = error "FIXME"
compileExp (IWord32_2 n m) = error "FIXME"
compileExp (IWord32_3 n m k) = error "FIXME"
compileExp (IWord32_4 n m k l) = error "FIXME"
compileExp (IWord64_2 n m) = error "FIXME"
compileExp (IWord64_3 n m k) = error "FIXME"
compileExp (IWord64_4 n m k l) = error "FIXME"


compileExp (IIndex (i1,[e]) t) = [cexp| $(compileExp i1)[$(compileExp e)] |]
compileExp a@(IIndex (_,_) _) = error $ "compileExp: Malformed index expression " ++ show a

compileExp (ICond e1 e2 e3 t) = [cexp| $(compileExp e1) ? $(compileExp e2) : $(compileExp e3) |]

compileExp (IBinOp op e1 e2 t) = go op 
  where
    x = compileExp e1
    y = compileExp e2
    go IAdd = [cexp| $x + $y |]
    go ISub = [cexp| $x - $y |]
    go IMul = [cexp| $x * $y |]
    go IDiv = [cexp| $x / $y |]
    go IFDiv = [cexp| $x / $y |]
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
                _      -> error $ "IPow applied at wrong type" 
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
    go IGetX       = [cexp| $x.x|]
    go IGetY       = [cexp| $x.y|]
    go IGetZ       = [cexp| $x.z|]
    go IGetW       = [cexp| $x.w|]
    
compileExp (IFunCall name es t) = [cexp| $fc |]
  where
    es' = map compileExp es
    fc  = [cexp| $id:(name)($args:(es')) |]

compileExp (ICast e t) = [cexp| ($ty:(compileType t)) $e' |]
  where
    e' = compileExp e

compileType :: T.Type -> C.Type
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

compileType (Vec2 Float) = [cty| float4|]
compileType (Vec3 Float) = [cty| float3|]
compileType (Vec4 Float) = [cty| float2|]

compileType (Vec2 Double) = [cty| double2|]

-- How does this interplay with my use of uint8_t etc. Here it is char!
compileType (Vec2 Int8) = [cty| char2|]
compileType (Vec3 Int8) = [cty| char3|]
compileType (Vec4 Int8) = [cty| char4|]
 
compileType (Vec2 Int16) = [cty| short2|]
compileType (Vec3 Int16) = [cty| short3|]
compileType (Vec4 Int16) = [cty| short4|]

compileType (Vec2 Int32) = [cty| int2|]
compileType (Vec3 Int32) = [cty| int3|]
compileType (Vec4 Int32) = [cty| int4|]

compileType (Vec2 Word8) = [cty| uchar2|]
compileType (Vec3 Word8) = [cty| uchar3|]
compileType (Vec4 Word8) = [cty| uchar4|]
 
compileType (Vec2 Word16) = [cty| ushort2|]
compileType (Vec3 Word16) = [cty| ushort3|]
compileType (Vec4 Word16) = [cty| ushort4|]

compileType (Vec2 Word32) = [cty| uint2|]
compileType (Vec3 Word32) = [cty| uint3|]
compileType (Vec4 Word32) = [cty| uint4|]


compileType (Shared t) = [cty| __local $ty:(compileType t) |]
compileType (Pointer t) = [cty| $ty:(compileType t)* |]
compileType (Volatile t) =  [cty| volatile $ty:(compileType t)|]
compileType t = error $ "compileType: Not implemented " ++ show t


---------------------------------------------------------------------------
-- Statement t to Stm
---------------------------------------------------------------------------

compileStm :: Config -> Statement t -> [C.Stm]
compileStm c (SAssign name [] e) =
   [[cstm| $(compileExp name) = $(compileExp e);|]]
compileStm c (SAssign name [ix] e) = 
   [[cstm| $(compileExp name)[$(compileExp ix)] = $(compileExp e); |]]
compileStm c (SAtomicOp name ix atop) = 
  case atop of
    AtInc -> [[cstm| atomicInc(&$(compileExp name)[$(compileExp ix)],0xFFFFFFFF); |]]
    AtAdd e -> [[cstm| atomicAdd(&$(compileExp name)[$(compileExp ix)],$(compileExp e));|]]
    AtSub e -> [[cstm| atomicSub(&$(compileExp name)[$(compileExp ix)],$(compileExp e));|]]
    AtExch e -> [[cstm| atomicExch(&$(compileExp name)[$(compileExp ix)],$(compileExp e));|]]

compileStm c (SCond be im) = [[cstm| if ($(compileExp be)) { $stms:body } |]]
  where 
    body = compileIM c im  -- (compileIM p c im)
compileStm c (SSeqFor loopVar n im) = 
    [[cstm| for (int $id:loopVar = 0; $id:loopVar < $(compileExp n); ++$id:loopVar) 
              { $stms:body } |]]
  where
    body = compileIM c im -- (compileIM p c im)


-- Just relay to specific compileFunction
compileStm c a@(SForAll lvl n im) = compileForAll c a

compileStm c a@(SDistrPar lvl n im) = compileDistr c a 

compileStm c (SSeqWhile b im) =
  [[cstm| while ($(compileExp b)) { $stms:body}|]]
  where
    body = compileIM c im 

-- compileStm c SSynchronize = [[cstm| __syncthreads(); |]]
compileStm c SSynchronize = [[cstm| barrier(CLK_LOCAL_MEM_FENCE); |]]
    

compileStm _ (SAllocate _ _ _) = []
compileStm _ (SDeclare name t) = []

compileStm _ a = error  $ "compileStm: missing case "

---------------------------------------------------------------------------
-- DistrPar 
---------------------------------------------------------------------------
compileDistr :: Config -> Statement t -> [C.Stm] 
compileDistr c (SDistrPar Block n im) =  codeQ ++ codeR
  -- New here is BLOCK virtualisation
  where
    cim = compileIM c im  -- ++ [[cstm| __syncthreads();|]]
    
    numBlocks = [cexp| get_num_groups(0) |]
    
    blocksQ = [cexp| $exp:(compileExp n) / $exp:numBlocks|]
    blocksR = [cexp| $exp:(compileExp n) % $exp:numBlocks|] 
    
    codeQ = [[cstm| for (int b = 0; b < $exp:blocksQ; ++b) { $stms:bodyQ }|]]
                
    bodyQ = [cstm| $id:("bid") = $exp:blockIdx_x * $exp:blocksQ + b;|] : cim  ++  
            [[cstm| bid = $exp:blockIdx_x;|],
             [cstm| barrier(CLK_LOCAL_MEM_FENCE);|]] -- yes no ? 
         
    codeR = [[cstm| bid = ($exp:numBlocks * $exp:blocksQ) + $exp:blockIdx_x;|], 
             [cstm| if ($exp:blockIdx_x < $exp:blocksR) { $stms:cim }|],
             [cstm| bid = $exp:blockIdx_x;|], 
             [cstm| barrier(CLK_LOCAL_MEM_FENCE);|]] -- yes no ? 
                    
-- Can I be absolutely sure that 'n' here is statically known ? 
-- I must look over the functions that can potentially create this IM. 
-- Can make a separate case for unknown 'n' but generate worse code.
-- (That is true for all levels)  
compileDistr c (SDistrPar Warp (IWord32 n) im) = 
  error "Compiling distribution over OpenCL wavefronts is not supported yet" 
  -- codeQ  ++ codeR 

  -- Here the 'im' should be distributed over 'n'warps.
  -- 'im' uses a warpID variable to identify what warp it is.
  -- 'n' may be higher than the actual number of warps we have!
  -- So GPU warp virtualisation is needed. 
  -- where
  --   cim = compileIM c im

  --   nWarps   = fromIntegral $ configThreadsPerBlock c `div` 32
  --   numWarps = [cexp| $int:nWarps|] 

  --   warpsQ   = [cexp| $int:(n `div` nWarps)|]
  --   warpsR   = [cexp| $int:(n `mod` nWarps)|]
    
  --   codeQ = [[cstm| for (int w = 0; w < $exp:warpsQ; ++w) { $stms:bodyQ } |]]
    
  --   bodyQ = [cstm| warpID = (threadIdx.x / 32) * $exp:warpsQ + w;|] : cim ++
  --           --[cstm| warpID = w * $exp:warpsQ + (threadIdx.x / 32);|] : cim ++ 
  --           [[cstm| warpID = threadIdx.x / 32;|]] 

  --   codeR = case (n `mod` nWarps)  of 
  --            0 -> [] 
  --            n -> [[cstm| warpID = ($exp:numWarps * $exp:warpsQ)+ (threadIdx.x / 32);|],
  --                  [cstm| if (threadIdx.x / 32 < $exp:warpsR) { $stms:cim } |], 
  --                  [cstm| warpID = threadIdx.x / 32; |], 
  --                  [cstm| __syncthreads();|]]

---------------------------------------------------------------------------
-- ForAll 
---------------------------------------------------------------------------
compileForAll :: Config -> Statement t -> [C.Stm]
compileForAll c (SForAll Warp  (IWord32 n) im) =
  error "Compiling distribution over OpenCL wavefronts is not supported yet"
  --  codeQ ++ codeR
  -- where
  --   nt = 32

  --   q = n `div` nt
  --   r = n `mod` nt

  --   cim = compileIM c im 
    
  --   codeQ =
  --     case q of
  --       0 -> []
  --       1 -> cim
  --       n -> [[cstm| for ( int vw = 0; vw < $int:q; ++vw) { $stms:body } |], 
  --             [cstm| $id:("warpIx") = threadIdx.x % 32; |]]
  --             --[cstm| __syncthreads();|]]
  --            where 
  --              body = [cstm|$id:("warpIx") = vw*$int:nt + (threadIdx.x % 32); |] : cim
  --              --body = [cstm|$id:("warpIx") = (threadIdx.x % 32) * q + vw; |] : cim

  --   codeR = 
  --     case r of 
  --       0 -> [] 
  --       n -> [[cstm| if ((threadIdx.x % 32) < $int:r) { 
  --                           $id:("warpIx") = $int:(q*32) + (threadIdx.x % 32);  
  --                           $stms:cim } |],
  --                 -- [cstm| __syncthreads();|],
  --                 [cstm| $id:("warpIx") = threadIdx.x % 32; |]]

compileForAll c (SForAll Block (IWord32 n) im) = goQ ++ goR 
  where
    cim = compileIM c im -- ++ [[cstm| __syncthreads();|]]
   
    nt = configThreadsPerBlock c 

    q  = n `quot` nt
    r  = n `rem`  nt 

    -- q is the number full "passes" needed to cover the iteration
    -- space given we have nt threads. 
    goQ =
      case q of
        0 -> []
        1 -> cim -- [cstm|$id:loopVar = threadIdx.x; |]:cim
            --do
            --  stm <- updateTid [cexp| threadIdx.x |]
            --  return $ [cstm| $id:loopVar = threadIdx.x; |] : cim 
        n -> [[cstm| for ( int i = 0; i < $int:q; ++i) { $stms:body } |], 
                                                     --  __syncthreads(); } |], 
              [cstm| $id:("tid") = $exp:threadIdx_x; |]]
          --    [cstm| __syncthreads();|]]
             where 
               body = [cstm|$id:("tid") =  i*$int:nt + $exp:threadIdx_x; |] : cim
   
    -- r is the number of elements left. 
    -- This generates code for when fewer threads are 
    -- needed than available. (some threads shut down due to the conditional). 
    goR = 
      case (r,q) of 
        (0,_) -> [] 
        --(n,0) -> [[cstm| if (threadIdx.x < $int:n) { 
        --                    $stms:cim } |]] 
        (n,m) -> [[cstm| if ($exp:threadIdx_x < $int:n) { 
                            $id:("tid") = $int:(q*nt) + $exp:threadIdx_x;  
                            $stms:cim } |], 
                  [cstm| $id:("tid") = $exp:threadIdx_x; |]]

compileForAll c (SForAll Grid n im) = error "compileForAll: Grid" -- cim
  -- The grid case is special. May need more thought
  -- 
  -- The problem with this case is that
  -- I need to come up with a blocksize (but without any guidance)
  -- from the programmer.
  -- Though! There is no way the programmer could provide any
  -- such info ... 
  where
    cim = compileIM c im

-- compileForAll PlatformC c (SForAll lvl (IWord32 n) im) = go
--   where
--     body = compileIM PlatformC c im 
--     go  = [ [cstm| for (int i = 0; i <$int:n; ++i) { $stms:body } |] ] 
      

--------------------------------------------------------------------------- 
-- CompileIM to list of Stm 
--------------------------------------------------------------------------- 
compileIM :: Config -> IMList a -> [C.Stm]
compileIM conf im = concatMap ((compileStm conf) . fst) im

---------------------------------------------------------------------------
-- Generate entire Kernel 
---------------------------------------------------------------------------
type Parameters = [(String,T.Type)]

compile :: Config -> String -> (Parameters,IMList a) -> C.Definition
compile config kname (params,im)
  = go  
  where
    stms = compileIM config im
    
    ps = compileParams params
    go = [cedecl| extern "C" __kernel void $id:kname($params:ps) {$items:cudabody} |]

    cudabody = (if (configSharedMem config > 0)
                then [C.BlockDecl
                      [cdecl| __local  typename uint8_t  sbase[$uint:(configSharedMem config)] ; |]] 
                else []) ++
               (if (usesGid im) 
                then [C.BlockDecl [cdecl| typename uint32_t gid = $exp:blockIdx_x * $exp:blockDim_x + $exp:threadIdx_x; |]]
                else []) ++ 
               (if (usesBid im) 
                then [C.BlockDecl [cdecl| typename uint32_t bid = $exp:blockIdx_x; |]]
                else []) ++ 
               (if (usesTid im) 
                then [C.BlockDecl [cdecl| typename uint32_t tid = $exp:threadIdx_x; |]]
                else []) ++
               (if (usesWarps im) 
                then error "Warps not supported: OpenCL"
--                    [C.BlockDecl
--                      [cdecl| typename uint32_t warpID = threadIdx.x / 32; |],
--                     C.BlockDecl
--                      [cdecl| typename uint32_t warpIx = threadIdx.x % 32; |]] 
                else []) ++
                -- All variables used will be unique and can be declared 
                -- at the top level 
                concatMap declares im ++ 
                -- Not sure if I am using language.C correctly. 
                -- Maybe compileSTM should create BlockStms ?
                -- TODO: look how Nikola does it. 
                map C.BlockStm stms

    cbody = -- add memory allocation 
            map C.BlockStm stms

-- Declare variables. 
declares :: (Statement t,t) -> [C.BlockItem]
declares (SDeclare name t,_) = [C.BlockDecl [cdecl| $ty:(compileType t)  $id:name;|]]
declares (SCond _ im,_) = concatMap declares im 
declares (SSeqWhile _ im,_) = concatMap declares im
declares (SForAll _ _ im,_) = concatMap declares im
declares (SDistrPar _ _ im,_) = concatMap declares im
declares (SSeqFor _ _  im,_) = concatMap declares im
declares _ = []


--------------------------------------------------------------------------- 
-- Parameter lists for functions  (kernel head) 
---------------------------------------------------------------------------
compileParams :: Parameters -> [C.Param]
-- compileParams PlatformOpenCL = map go
--   where

-- C or CUDA 
compileParams = map go
  where
    go (name,Pointer t) = [cparam| global  $ty:(compileType (Pointer t)) $id:name |]
    go (name, t)        = [cparam| $ty:(compileType t) $id:name |]
    

 

---------------------------------------------------------------------------
-- Compile with shared memory arrays declared at top
---------------------------------------------------------------------------
-- CODE DUPLICATION FOR NOW

compileDeclsTop :: Config -> [(String,((Word32,Word32),T.Type))] -> String -> (Parameters,IMList a) -> C.Definition
compileDeclsTop config toplevelarrs kname (params,im)
  = go 
  where
    stms = compileIM config im
    
    ps = compileParams params
    go = [cedecl| extern "C" __kernel void $id:kname($params:ps) {$items:cudabody} |]
    cudabody = (if (configSharedMem config > 0)
                -- then [BlockDecl [cdecl| extern volatile __shared__  typename uint8_t sbase[]; |]] 
                then [C.BlockDecl [cdecl| __local typename uint8_t sbase[$uint:(configSharedMem config)]; |]] 
                else []) ++
                --[BlockDecl [cdecl| typename uint32_t tid = threadIdx.x; |]] ++
                --[BlockDecl [cdecl| typename uint32_t warpID = threadIdx.x / 32; |],
                --       BlockDecl [cdecl| typename uint32_t warpIx = threadIdx.x % 32; |]] ++
--                [BlockDecl [cdecl| typename uint32_t bid = blockIdx.x; |]] ++
               (if (usesGid im) 
                then [C.BlockDecl [cdecl| typename uint32_t gid = $exp:blockIdx_x * $exp:blockDim_x + $exp:threadIdx_x; |]]
                else []) ++ 
               (if (usesBid im) 
                then [C.BlockDecl
                       [cdecl| typename uint32_t bid = $exp:blockIdx_x; |]]
                else []) ++ 
               (if (usesTid im) 
                then [C.BlockDecl
                       [cdecl| typename uint32_t tid = $exp:threadIdx_x; |]]
                else []) ++
               (if (usesWarps im) 
                then error "\"Warps\" no implemented in the OpenCL code-generator" 
                    --[C.BlockDecl
                    --   [cdecl| typename uint32_t warpID = threadIdx.x / 32; |],
                    -- C.BlockDecl
                    --   [cdecl| typename uint32_t warpIx = threadIdx.x % 32; |]] 
                else []) ++
                -- declare all arrays used
                concatMap declareArr toplevelarrs ++
                -- All variables used will be unique and can be declared 
                -- at the top level 
                concatMap declares im ++ 
                -- Not sure if I am using language.C correctly. 
                -- Maybe compileSTM should create BlockStms ?
                -- TODO: look how Nikola does it. 
                map C.BlockStm stms

    cbody = -- add memory allocation 
            map C.BlockStm stms


declareArr :: (String, ((Word32,Word32),T.Type)) -> [C.BlockItem]
declareArr (arr,((_,addr),t)) =
  [C.BlockDecl [cdecl| $ty:(compileType (Shared t)) $id:arr = ( $ty:(compileType (Shared t)))(sbase + $int:addr);|]]

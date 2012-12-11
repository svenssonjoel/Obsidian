{-# LANGUAGE TypeOperators,
             GADTs #-} 

{- Joel Svensson 2012 -}
module Obsidian.CodeGen.CUDA.WithCUDA where


import qualified Data.Vector.Storable as V

import Obsidian.Types
import Obsidian.CodeGen.InOut

import Data.Word


---------------------------------------------------------------------------
-- A CUDA Ast for Printing or running.. 
---------------------------------------------------------------------------
type Id = Integer

data CUDADir = HostToDevice | DeviceToHost | DeviceToDevice

data CUDAProgram a where
  CUDANewId     :: CUDAProgram Id 
  CUDAKernel    ::  ToProgram a b
                    => (a -> b)
                    -> Ips a b
                    -> CUDAProgram Id
  CUDAUseVector :: (Show a, V.Storable a)
                   => Id 
                   -> V.Vector a
                   -> Type 
                   -> CUDAProgram ()

  CUDACopyVector :: Id -> Id -> Int -> CUDADir -> CUDAProgram ()
  CUDAAllocaVector :: Id 
                      -> Int
                      -> Type 
                      -> CUDAProgram ()

  CUDAFree :: Id -> CUDAProgram () 

  CUDAExecute :: Id 
                 -> Word32 -- Number of blocks
                 -> Word32 -- Amount of Shared mem (get from an analysis) 
                 -> [Id] -- identify inputs.
                 -> [Id] -- identfy outputs. 
                 -> CUDAProgram ()

 
  CUDATime :: String -> CUDAProgram () -> CUDAProgram () -- TimeVal  
  CUDABind :: CUDAProgram a
              -> (a -> CUDAProgram b)
              -> CUDAProgram b
  CUDAReturn :: a -> CUDAProgram a
  
---------------------------------------------------------------------------
-- Monad Instance
---------------------------------------------------------------------------
instance Monad CUDAProgram where
  return = CUDAReturn
  (>>=)  = CUDABind 

---------------------------------------------------------------------------
-- Operations
---------------------------------------------------------------------------
cudaCapture :: ToProgram a b => (a -> b) -> Ips a b -> CUDAProgram Id
cudaCapture f inputs = CUDAKernel f inputs
    {- 
    let kn      = "gen" ++ show id
        prgstr  = genKernel kn f inputs
        threads = getNThreads f inputs 
        header  = "#include <stdint.h>\n" -- more includes ? 
         
    CUDAKernel (header ++ prgstr)        
    return (kn,threads)
    -} 
cudaAlloca :: Int -> Type -> CUDAProgram Id
cudaAlloca size typ =
  do 
    id <- CUDANewId
    CUDAAllocaVector id (size * typeSize typ)  typ
    return id

cudaUseVector :: (Show a, V.Storable a) => V.Vector a -> Type -> CUDAProgram Id
cudaUseVector v typ =
  do
    hostid <- CUDANewId
    devid  <- CUDANewId
    CUDAUseVector hostid v typ
    CUDAAllocaVector devid (V.length v) typ
    CUDACopyVector devid hostid (V.length v * typeSize typ) HostToDevice
    return devid


cudaExecute :: Id {- (String, Word32) -}
               -> Word32
               -> Word32
               -> [Id]
               -> [Id]
               -> CUDAProgram ()
cudaExecute kern blocks sm ins outs =
  CUDAExecute kern blocks sm ins outs


cudaTime :: String -> CUDAProgram () -> CUDAProgram ()
cudaTime str prg = CUDATime str prg  


typeSize Int8 = 1
typeSize Int16 = 2
typeSize Int32 = 4
typeSize Int64 = 8
typeSize Word8 = 1
typeSize Word16 = 2
typeSize Word32 = 4
typeSize Word64 = 8
typeSize Bool = 4
typeSize Float = 4
typeSize Double = 8 

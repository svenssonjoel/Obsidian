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

data Mem = Host | Device 

-- Making this first order.. 
data CUDAProgram a where
  CUDAKernel    ::  ToProgram a b
                    => (a -> b)
                    -> Ips a b
                    -> CUDAProgram Kernel 

  -- This is one of those awkward cases where
  -- the data is carried in the AST. (But what is the alternative)
  -- This represents (COPY-IN from Haskell world) 
  CUDAUseVector :: (Show a, V.Storable a)
                   => V.Vector a
                   -> CUDAProgram (CUDAVector a)  

  CUDAAllocaVector :: Mem
                      -> Int
                      -> CUDAProgram (CUDAVector a)
  CUDACopy :: (IsVector v1, IsVector v2)
              => v1 a -> v2 a
              -> CUDAProgram () 

  CUDAExecute :: (ParamList a, ParamList b)
                 => Kernel 
                 -> Word32 -- Number of blocks
                 -> Word32
                 -> a -- inputs
                 -> b -- outputs 
                 -> CUDAProgram ()

  CUDAFree :: CUDAVector a -> CUDAProgram () 
 
  CUDATime :: String -> CUDAProgram () -> CUDAProgram () -- TimeVal  
  CUDABind :: CUDAProgram a
              -> (a -> CUDAProgram b)
              -> CUDAProgram b
  CUDAReturn :: a -> CUDAProgram a

---------------------------------------------------------------------------
-- Some types 
---------------------------------------------------------------------------

data CUDAVector a = CUDAVector Id

data HOSTVector a = HOSTVector Id 

class IsVector v where
  isHostVector :: v a -> Bool
  getVectorId  :: v a -> Id

instance IsVector CUDAVector where
  isHostVector _ = False
  getVectorId  (CUDAVector i) = i 

instance IsVector HOSTVector where
  isHostVector _ = True
  getVectorId (HOSTVector i) = i 

data Kernel = Kernel Id 


---------------------------------------------------------------------------
-- ParamList
---------------------------------------------------------------------------

data FunParam where
   VArg :: a -> FunParam 

class ParamList a where
  toParamList :: a -> [FunParam]

instance ParamList (CUDAVector a) where
  toParamList a = [VArg a]

instance (ParamList a, ParamList b) => ParamList (a :-> b) where
  toParamList (a :-> b) = toParamList a ++ toParamList b 

---------------------------------------------------------------------------
-- Monad Instance
---------------------------------------------------------------------------
instance Monad CUDAProgram where
  return = CUDAReturn
  (>>=)  = CUDABind 

---------------------------------------------------------------------------
-- Operations
---------------------------------------------------------------------------
cudaCapture :: ToProgram a b => (a -> b) -> Ips a b -> CUDAProgram Kernel 
cudaCapture f inputs = CUDAKernel f inputs

cudaAlloca :: Int -> Type -> (CUDAVector a -> CUDAProgram b) -> CUDAProgram b
cudaAlloca size typ f =
  do
    v <- CUDAAllocaVector Device (size * typeSize typ)
    r <- f v
    CUDAFree v
    return r 
    
cudaUseVector :: (Show a, V.Storable a) => V.Vector a -> Type
                 -> (CUDAVector a -> CUDAProgram b) -> CUDAProgram b
cudaUseVector v t f =
  do 
    dv <- CUDAUseVector v
    r <- f dv
    CUDAFree dv
    return r 

cudaExecute :: (ParamList a, ParamList b) => Kernel  
               -> Word32
               -> Word32
               -> a
               -> b
               -> CUDAProgram ()
cudaExecute kern blocks sm ins outs =
  CUDAExecute kern blocks sm ins outs


cudaTime :: String -> CUDAProgram () -> CUDAProgram ()
cudaTime str prg = CUDATime str prg  


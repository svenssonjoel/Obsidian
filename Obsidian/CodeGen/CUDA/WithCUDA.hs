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

--data CUDADir = HostToDevice | DeviceToHost | DeviceToDevice

-- Making this first order.. 
data CUDAProgram a where
--   CUDANewId     :: CUDAProgram Id 
  CUDAKernel    ::  ToProgram a b
                    => (a -> b)
                    -> Ips a b
                    -> CUDAProgram Kernel 
 
  CUDAUseVector :: (Show a, V.Storable a)
                   => V.Vector a
                   -> CUDAProgram (CUDAVector a)  

  CUDAAllocaVector :: Int
                      -> CUDAProgram (CUDAVector a) 

  CUDAExecute :: (ParamList a, ParamList b) => Kernel 
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
-- ParamList
---------------------------------------------------------------------------

data CUDAVector a = CUDAVector Id

data Kernel = Kernel Id 

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
    CUDAAllocaVector (size * typeSize typ) typ  f
    
cudaUseVector :: (Show a, V.Storable a) => V.Vector a -> Type
                 -> (CUDAVector a -> CUDAProgram b) -> CUDAProgram b
cudaUseVector v t f = CUDAUseVector v t f 

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


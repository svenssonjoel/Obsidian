

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Main where

import Obsidian

import Prelude hiding (zipWith)

-- For OpenCL printing 
import qualified Obsidian.CodeGen.OpenCLEmbedded as CL 

import Data.Word


vadd_wg :: Num a => Pull Word32 a -> Pull Word32 a -> Pull Word32 a
vadd_wg = zipWith (+) 



vadd :: Num a => Pull EWord32 a -> Pull EWord32 a -> Push Grid EWord32 a
vadd x y = asGrid $ zipWith body (splitUp 128 x) (splitUp 128 y)
  where
    body a b = push $ vadd_wg a b 



main = do 
  let str =
        CL.genKernel 128
                     "vadd" (vadd :: DPull (EInt32)
                                  -> DPull (EInt32) 
                                  -> DPush Grid (EInt32) )
  putStrLn str 
  

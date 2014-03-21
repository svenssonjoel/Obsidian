{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fractals where

-- import qualified Obsidian.CodeGen.CUDA as CUDA

import Obsidian

import Data.Word
import Data.Int
import Data.Bits

import Control.Monad.State

import Prelude hiding (zipWith,sum,replicate,take,drop,iterate)
import qualified Prelude as P


---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
-- Mandel
xmax =  1.2 :: EFloat
xmin = -2.0 :: EFloat
ymax =  1.2 :: EFloat
ymin = -1.2 :: EFloat

-- plate1
-- xmax =  -0.690906 :: EFloat
-- xmin =  -0.691060 :: EFloat
-- ymax =  0.387228 :: EFloat
-- ymin =  0.387103 :: EFloat

-- Plate2
-- xmax =  -0.723005 :: EFloat
-- xmin =  -0.793114 :: EFloat
-- ymax =  0.140974 :: EFloat
-- ymin =  0.037822 :: EFloat

-- Plate3 
-- xmax =  -0.745388 :: EFloat
-- xmin =  -0.745464 :: EFloat
-- ymax =  0.113030 :: EFloat
-- ymin =  0.112967 :: EFloat


-- For generating a 512x512 image
deltaP = (xmax - xmin) / 512.0
deltaQ = (ymax - ymin) / 512.0


f bid tid (x,y,iter) = (xsq - ysq + (xmin + (w32ToF tid) * deltaP),
                        2*x*y + (ymax - (w32ToF bid) * deltaQ),
                        iter+1) 
  where
    xsq = x*x
    ysq = y*y

cond (x,y,iter) = ((xsq + ysq) <* 4) &&* iter <* 512  
  where
    xsq = x*x
    ysq = y*y 


iters :: EWord32 -> EWord32 -> TProgram (SPush Thread EWord8)
iters bid tid =
  return $ fmap extract (seqUntil (f bid tid) cond  (0,0,1))
  where
    extract (_,_,c) = ((w32ToW8 c) `mod` 16) * 16


genRect :: forall b. MemoryOps b
           => EWord32
           -> Word32
           -> (EWord32 -> EWord32 -> SPush Thread b)
           -> DPush Grid b 
genRect bs ts p = generate bs $
                  \bid -> (tDistribute ts $ p bid  :: SPush Block b)

mandel = genRect 512 512 (runPush2 iters)  

getMandel = putStrLn $
            fst $
            genKernelSpecsNL 512 "mandel" mandel
  
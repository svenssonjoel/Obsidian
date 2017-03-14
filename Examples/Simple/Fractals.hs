{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fractals where

import Obsidian
import Obsidian.CodeGen.CUDA

import Data.Word

import Control.Monad.State

import Prelude hiding (zipWith,sum,replicate,take,drop,iterate,(<*))

---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
-- Mandel
xmin, xmax, ymin, ymax :: EFloat
xmax =  1.2 
xmin = -2.0 
ymax =  1.2 
ymin = -1.2 

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
deltaP, deltaQ :: EFloat 
deltaP = (xmax - xmin) / 512.0
deltaQ = (ymax - ymin) / 512.0

f :: EFloat
  -> EFloat
  -> (EFloat, EFloat, EWord32)
  -> (EFloat, EFloat, EWord32) 
f b t (x,y,iter) =
  (xsq - ysq + (xmin + t * deltaP),
   2*x*y + (ymax - b * deltaQ),
   iter+1) 
  where
    xsq = x*x
    ysq = y*y

cond :: (EFloat, EFloat, EWord32) -> EBool
cond (x,y,iter) = ((xsq + ysq) <* 4) &&* iter <* 512  
  where
    xsq = x*x
    ysq = y*y 


iters :: EWord32 -> EWord32 -> Program Thread EW8 
iters bid tid =
  do (_,_,c) <- seqUntil (f bid' tid') cond  (0,0,1)
     return (color c) 
  where
    color c = (w32ToW8 (c `mod` 16)) * 16
    tid' = w32ToF tid
    bid' = w32ToF bid 

genRect :: EWord32 
           -> Word32
           -> (EWord32 -> EWord32 -> SPush Thread b)
           -> DPush Grid b 
genRect bs ts p = asGrid 
                $ mkPull bs 
                $ \bid -> asBlock $ mkPull ts (p bid)

mandel :: DPush Grid EW8
mandel = genRect 512 512 body 
  where 
    body i j = execThread' (iters i j) 

getMandel :: IO ()
getMandel = putStrLn $
            genKernel 512 "mandel" mandel
  

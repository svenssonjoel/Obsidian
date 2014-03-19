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
-- Util 
---------------------------------------------------------------------------
--quickPrint :: ToProgram a => a -> InputList a -> IO ()
--quickPrint prg input =
--  putStrLn $ CUDA.genKernel "kernel" prg input 


---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------
--splitUp :: Word32 -> DPull a -> DPull (SPull a)
--splitUp n (Pull m ixf) = Pull (m `div` fromIntegral n) $ 
--                          \i -> Pull n $ \j -> ixf (i * (sizeConv n) + j)




---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
-- Mandel
-- xmax =  1.2 :: EFloat
-- xmin = -2.0 :: EFloat
-- ymax =  1.2 :: EFloat
-- ymin = -1.2 :: EFloat

-- plate1
-- xmax =  -0.690906 :: EFloat
-- xmin =  -0.691060 :: EFloat
-- ymax =  0.387228 :: EFloat
-- ymin =  0.387103 :: EFloat

-- Plate2
xmax =  -0.723005 :: EFloat
xmin =  -0.793114 :: EFloat
ymax =  0.140974 :: EFloat
ymin =  0.037822 :: EFloat

-- Plate3 
-- xmax =  -0.745388 :: EFloat
-- xmin =  -0.745464 :: EFloat
-- ymax =  0.113030 :: EFloat
-- ymin =  0.112967 :: EFloat



-- For generating a 512x512 image
deltaP = (xmax - xmin) / 512.0
deltaQ = (ymax - ymin) / 512.0

-- f bid tid iter (x,y,_) = (xsq - ysq + (xmin + (word32ToFloat tid) * deltaP),
--                           2*x*y + (ymax - (word32ToFloat bid) * deltaQ),
--                           word32ToWord8 iter) 
--   where
--     xsq = x*x
--     ysq = y*y

-- cond _ (x,y,_) = (xsq + ysq) >=* 4
--   where
--     xsq = x*x
--     ysq = y*y 

-- iters bid tid = liftM extract $  seqUntilBound 513 (f bid tid) cond  (0,0,0)
--   where
--     extract (_,_,c) = (c `mod` 16) * 16

-- mandel = genB 512 $ (\bid -> force $ genT 512 (iters bid)) 

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


-- iters bid tid = return $ Push 1 $ \wf ->
--   do
--     a <- seqUntil (f bid tid) cond  (0,0,1)
--     wf (extract a) 0
--   where
--     extract (_,_,c) = ((word32ToWord8 c) `mod` 16) * 16
iters :: EWord32 -> EWord32 -> TProgram (SPush Thread EWord8)
iters bid tid =
  return $ fmap extract (seqUntil (f bid tid) cond  (0,0,1))
  where
    extract (_,_,c) = ((w32ToW8 c) `mod` 16) * 16

-- genRect :: MemoryOps b
--            => EWord32
--            -> Word32 -> (EWord32 -> EWord32 -> TProgram b) -> DPush Grid b
-- genRect bs ts p = genB bs (\bid -> return $ genT ts (p bid))

genRect :: forall b. MemoryOps b
           => EWord32
           -> Word32
           -> (EWord32 -> EWord32 -> SPush Thread b)
           -> DPush Grid b 
genRect bs ts p = generate bs $
                  \bid -> (tDistribute ts $ p bid  :: SPush Block b)

-- mandel = generateB 512 $ \bid -> return $ generateT 512 (iters bid)
mandel = genRect 512 512 (\b -> pJoin . iters b)

getMandel = putStrLn $
            fst $
            genKernelSpecsNL 512 "mandel" mandel
  
  -- quickPrint mandel ()
--getMandel

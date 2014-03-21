
{-# LANGUAGE ScopedTypeVariables #-}
module Main where




import Prelude hiding (replicate)
import Prelude as P


import Obsidian
import Obsidian.Run.CUDA.Exec


import qualified Data.Vector.Storable as V
import Control.Monad.State


import Data.Int
import Data.Word


test1_local :: SPull EWord32 -> SPush Block EWord32
test1_local arr =  push arr

test1 :: Word32 -> DPull EWord32 -> DPush Grid EWord32
test1 n arr = pConcat (fmap test1_local (splitUp n arr))


runTest1 =
  withCUDA $
  do
    kern <- capture 128 (test1 1024) 

    (inputs :: V.Vector Word32) <- lift $ mkRandomVec (1024 * 1024)

    useVector inputs $ \i ->
      allocaVector (1024 * 1024) $ \o ->
      do
        o <== (1024,kern) <> i
        r <- peekCUDAVector o
        lift $ putStrLn $ show r 

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Scan
import Reduction

import Prelude hiding (replicate)
import Prelude as P


import Obsidian
import Obsidian.Run.CUDA.Exec

import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Word

import System.Random 


perform =
  withCUDA $
  do
  
    scanI <- capture 256 (\a b -> sklanskyInc 8 (+) a (splitUp 256 b))
    reduce <- capture 256 (reduce (+)) --  . splitUp 256) 
    scanCin <- capture 256 kernel 

    gen <- lift $ newStdGen
  
    let input = P.take 65536 $ randomRs (0,10) gen -- [0..65535] -- P.replicate 65536 (1::Word32)

    useVector (V.fromList input) $ \i -> 
      allocaVector 256 $ \ (reds :: CUDAVector Word32) ->
        allocaVector 65536 $ \ (o :: CUDAVector Word32) ->
        do
          reds <== (256,reduce) <> i
          
          reds <== (1,scanI) <> (0 :: Word32) <> reds 

          o <== (256,scanCin) <> reds <> i
          
          r <- peekCUDAVector o
          lift $ putStrLn $ show (P.take 256 r)
          lift $ putStrLn "..."
          lift $ putStrLn $ show (P.drop 65280 r)

          -- Compare to CPU results. 
          let cpuRes = P.scanl1 (+) input 
          lift $ putStrLn $ "compare results (CPU == GPU): " ++ show (cpuRes == r)
  where
    kernel cins arr = sklanskyCin 8 (+) cins (splitUp 256 arr)

                      

main = perform 

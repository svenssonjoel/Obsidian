
{-# LANGUAGE ScopedTypeVariables #-}

module ReductionExec where

import Reduce

import Prelude hiding (replicate)
import Prelude as P


import Obsidian
import Obsidian.Run.CUDA.Exec
-- import Obsidian.Run.CUDA.SC

import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Int
import Data.Word

-- I cannot get Data.Vector.Random to install (error!) 
-- import Data.Vector.Random
import System.Random

perform512 r = 
  withCUDA $
  do
    kern <- capture 256 (r (+) . splitUp 512)

    g <- lift $ newStdGen

    let (inputs :: [Word32]) = P.take 512 $ randoms g
        cpuresult = sum inputs 
    
    useVector (V.fromList inputs) $ \i ->
      allocaVector 1 $ \o -> 
      do
        o <== (1,kern) <> i 
        r <- peekCUDAVector o
        lift $ putStrLn $ show r
        lift $ putStrLn $ "compare CPU GPU results equal?: " ++ show ((r P.!! 0) == cpuresult)

performRed1_1 = perform512 mapRed1
  
performRed2_1 = perform512 mapRed2

performRed3_1 = perform512 mapRed3

performRed4_1 = perform512 mapRed4

performRed5_1 = perform512 mapRed5

allSmall = [performRed1_1, performRed2_1, performRed3_1, performRed4_1, performRed5_1]

performAll = do
  sequence_ allSmall

-- performLarge =
--   withCUDA $
--   do
--     kern <- capture 256 (reduce (+)) --  . splitUp 256) 

--     useVector (V.fromList [0..65535 :: Int32]) $ \i ->
--       allocaVector (256)  $ \(o :: CUDAVector Int32) ->
--         allocaVector 1  $ \(o2 :: CUDAVector Int32) -> 
--         do
--           fill o 0 
--           o <== (256,kern) <> i
--           o2 <== (1,kern) <> o 

--           r <- peekCUDAVector o2
--           lift $ putStrLn $ show r 



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


perform4096 r = 
  withCUDA $
  do
    kern <- capture 256 (r (+) . splitUp 4096)

    g <- lift $ newStdGen

    let (inputs :: [Word32]) = P.take 4096 $ randoms g
        cpuresult = sum inputs 
    
    useVector (V.fromList inputs) $ \i ->
      allocaVector 1 $ \o -> 
      do
        o <== (1,kern) <> i 
        r <- peekCUDAVector o
        lift $ putStrLn $ show r
        lift $ putStrLn $ "compare CPU GPU results equal?: " ++ show ((r P.!! 0) == cpuresult)



all512 = [perform512 mapRed1,
          perform512 mapRed2,
          perform512 mapRed3,
          perform512 mapRed4,
          perform512 mapRed5,
          perform512 mapRed6,
          perform512 mapRed7]

all4096 = [perform4096 mapRed1,
           perform4096 mapRed2,
           perform4096 mapRed3,
           perform4096 mapRed4,
           perform4096 mapRed5,
           perform4096 mapRed6,
           perform4096 mapRed7]

           

performAll512 = do
  sequence_ all512

performAll4096 = do
  sequence_ all4096

-- ######################################################################
-- 
-- ######################################################################



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


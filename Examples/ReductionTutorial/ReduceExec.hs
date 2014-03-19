
{-# LANGUAGE ScopedTypeVariables #-}

module ReduceExec where

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
import System.Random.Mersenne.Pure64
import Data.Vector.Random.Mersenne
-- import System.Random

perform512 r = 
  withCUDA $
  do
    kern <- capture 256 (r (+) . splitUp 512)


    mt <- lift $ newPureMT

    let inputs = (randoms mt 512  :: V.Vector Word32) -- P.take 512 $ randoms g
        cpuresult = V.sum inputs 
    
    useVector inputs $ \i ->
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

    mt <- lift $ newPureMT 

    let inputs = (randoms mt 4096 :: V.Vector Word32) -- P.take 4096 $ randoms g
        cpuresult = V.sum inputs 
    
    useVector inputs $ \i ->
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

           

performAll512 = sequence_ all512

performAll4096 = sequence_ all4096

-- ######################################################################
-- Large reductions (Multiblock reductions) 
-- ######################################################################

performLarge r =
  withCUDA $
  do
    kern <- capture 256 (r (+) . splitUp 512) 

    mt <- lift $ newPureMT 

    let inputs = (randoms mt 262144 :: V.Vector Word32) --  P.take 262144 $ randoms g
        cpuresult = V.sum inputs 
    
    useVector inputs $ \i ->
      allocaVector 512  $ \(o :: CUDAVector Word32) ->
        allocaVector 1  $ \(o2 :: CUDAVector Word32) -> 
        do
          fill o 0 
          o <== (512,kern) <> i
          o2 <== (1,kern) <> o 

          r <- peekCUDAVector o2
          lift $ putStrLn $ show r
          lift $ putStrLn $ "compare CPU GPU results equal?: " ++ show ((r P.!! 0) == cpuresult)

allLarge = [performLarge mapRed1,
            performLarge mapRed2,
            performLarge mapRed3,
            performLarge mapRed4,
            performLarge mapRed5,
            performLarge mapRed6,
            performLarge mapRed7]

performAllLarge = sequence_ allLarge

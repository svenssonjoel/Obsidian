
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (replicate)
import Prelude as P


import Obsidian
import Obsidian.Run.CUDA.Exec
-- import Obsidian.Run.CUDA.SC

import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Int
import Data.Word

import System.Environment
import System.CPUTime.Rdtsc (rdtsc)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Data.IORef

import Data.Time.Clock

-- ######################################################################
-- Reduction Kernel 
-- ######################################################################
red :: Storable a
       => Word32 
       -> (a -> a -> a)
       -> Pull Word32 a
       -> Program Block a
red cutoff f  arr
  | len arr == cutoff =
    return (fold1 f arr ! 0) 
  | otherwise = 
    do
      let (a1,a2) = halve arr
      arr' <- forcePull (zipWith f a1 a2)
      red3 cutoff f arr'   


mapRed :: Storable a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapRed f arr = pConcat (fmap body arr)
  where
    body arr = singletonPush (red 2 f arr)




-- ######################################################################
-- Main
-- ######################################################################



main :: IO ()
main = putStrLn "hello World" 

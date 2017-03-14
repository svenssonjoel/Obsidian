{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import SortNet

import Obsidian
import Obsidian.Run.CUDA.Exec


import qualified Data.Vector.Storable as V
import Control.Monad.State

import Data.Int
import Data.Word
import Data.List (sort) 

import System.Exit 

import Data.Time.Clock

import Prelude hiding (reverse,(<*)) 
import qualified Prelude as P 

perform :: IO ()
perform =
  withCUDA $
  do
    compile_t0 <- lift getCurrentTime 
    kern <- capture 32 mapTest
    compile_t1 <- lift getCurrentTime 

    (inputs' :: V.Vector Word32) <- lift $ mkRandomVec 1024
    let inputs = V.map (`mod` 64) inputs'
--    let inputs = V.fromList (P.reverse [0..1023::Word32])

    let sorted = sort (V.toList inputs) 
    
    transfer_start <- lift getCurrentTime 
    useVector inputs $ \i ->
      allocaVector 1024 $ \ o ->
      do
        fill o 0
        transfer_done <- lift getCurrentTime

        t0 <- lift getCurrentTime
        forM_ [0..999] $ \ _ -> do
          o <== (1,kern) <> i
          syncAll 
        t1 <- lift getCurrentTime 
        
        r <- peekCUDAVector o

        t_end <- lift getCurrentTime 

        lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
--        lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)

        lift $ putStrLn $ "COMPILATION_TIME: " ++ show (diffUTCTime compile_t1 compile_t0)
    
--        lift $ putStrLn $ "BYTES_TO_DEVICE: " ++ show (fromIntegral (blcks * elts)  * sizeOf (undefined :: EWord32))
--        lift $ putStrLn $ "BYTES_FROM_DEVICE: " ++ show (fromIntegral blcks * sizeOf (undefined :: EWord32))
        lift $ putStrLn $ "TRANSFER_TO_DEVICE: " ++ show (diffUTCTime transfer_done transfer_start)
        lift $ putStrLn $ "TRANSFER_FROM_DEVICE: " ++ show (diffUTCTime t_end t1)

--        lift $ putStrLn $ "ELEMENTS_PROCESSED: " ++ show (fromIntegral (blcks * elts))
--        lift $ putStrLn $ "NUMBER_OF_BLOCKS: " ++ show (fromIntegral blcks)
--        lift $ putStrLn $ "ELEMENTS_PER_BLOCK: " ++ show (fromIntegral elts) 



        lift $ putStrLn $ show r 

        if isSorted r
          then
          do
            if (sorted == r)
              then lift $ putStrLn $ "CPU/GPU Same? "  ++ show (sorted == r)
              else lift $ exitFailure 
            lift $ putStrLn "Sorting: Success" 
            lift $ exitSuccess 
          else
          do
            lift $ putStrLn "Sorting: Failure"
            lift $ exitFailure


performParam :: Word32 -> Word32 -> IO ()
performParam w b =
  withCUDA $
  do
    compile_t0 <- lift getCurrentTime 
    kern <- capture 32 (mapTestParam w b) 
    compile_t1 <- lift getCurrentTime 

    (inputs' :: V.Vector Word32) <- lift $ mkRandomVec 1024
    let inputs = V.map (`mod` 64) inputs'
--    let inputs = V.fromList (P.reverse [0..1023::Word32])

    let sorted = sort (V.toList inputs) 
    
    transfer_start <- lift getCurrentTime 
    useVector inputs $ \i ->
      allocaVector 1024 $ \ o ->
      do
        fill o 0
        transfer_done <- lift getCurrentTime

        t0 <- lift getCurrentTime
        forM_ [0..999] $ \ _ -> do
          o <== (1,kern) <> i
          syncAll 
        t1 <- lift getCurrentTime 
        
        r <- peekCUDAVector o

        t_end <- lift getCurrentTime 

        lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
--        lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)

        lift $ putStrLn $ "COMPILATION_TIME: " ++ show (diffUTCTime compile_t1 compile_t0)
    
--        lift $ putStrLn $ "BYTES_TO_DEVICE: " ++ show (fromIntegral (blcks * elts)  * sizeOf (undefined :: EWord32))
--        lift $ putStrLn $ "BYTES_FROM_DEVICE: " ++ show (fromIntegral blcks * sizeOf (undefined :: EWord32))
        lift $ putStrLn $ "TRANSFER_TO_DEVICE: " ++ show (diffUTCTime transfer_done transfer_start)
        lift $ putStrLn $ "TRANSFER_FROM_DEVICE: " ++ show (diffUTCTime t_end t1)

--        lift $ putStrLn $ "ELEMENTS_PROCESSED: " ++ show (fromIntegral (blcks * elts))
--        lift $ putStrLn $ "NUMBER_OF_BLOCKS: " ++ show (fromIntegral blcks)
--        lift $ putStrLn $ "ELEMENTS_PER_BLOCK: " ++ show (fromIntegral elts) 



        lift $ putStrLn $ show r 

        if isSorted r
          then
          do
            if (sorted == r)
              then lift $ putStrLn $ "CPU/GPU Same? "  ++ show (sorted == r)
              else lift $ exitFailure 
            lift $ putStrLn "Sorting: Success" 
          else
          do
            lift $ putStrLn "Sorting: Failure"
            lift $ exitFailure



isSorted [] = True
isSorted [x] = True 
isSorted (x:xs) = x <= minimum xs && isSorted xs

main :: IO () 
main = do
  mapM_ (\(w,b) -> performParam w b)
    [(32,512), (64,512), (128,512), (256,512)
    ,(32,256), (64,256), (128,256), (256,256)
    ,(32,128), (64,128), (128,128), (256,128)
    ,(32,64), (64,64), (128,64), (256,64)]                       
  perform 

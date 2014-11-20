
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding ( replicate, zipWith ) 
import qualified Prelude as P


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
import Control.DeepSeq

-- ######################################################################
-- Reduction Kernel 
-- ######################################################################
red3 :: Storable a
           => Word32 
           -> (a -> a -> a)
           -> Pull Word32 a
           -> Program Block a
red3 cutoff f  arr
  | len arr == cutoff =
    return (fold1 f arr ! 0) 
  | otherwise = 
    do
      let (a1,a2) = halve arr
      arr' <- forcePull (zipWith f a1 a2)
      red3 cutoff f arr'   


mapRed3 :: Storable a => Word32 -> Word32 -> (a -> a -> a) -> DPull a -> DPush Grid a
mapRed3 blocksize seq_depth f arr = pConcat $ fmap sConcat (fmap (fmap body) arr')
  where
    body arr = singletonPush (red3 2 f arr)
    arr' =  fmap (splitUp (blocksize `div` seq_depth)) (splitUp blocksize arr)


-- ######################################################################
-- Nonsense Kernel 
-- ######################################################################
nonsense :: (Storable a, Num a) =>
            Bool 
           -> Pull Word32 a
           -> BProgram (Push Block Word32 a)
nonsense sync arr = do
  arr' <- loop 9 arr    -- same depth complexity as scan (Maybe vary this!) 
  return $ push arr
  where
    loop :: (Storable a, Num a) => Int -> Pull Word32 a -> BProgram (Pull Word32 a)
    loop 0 ain = return ain
    loop n ain = do
      a' <- force' $ fmap (+1) ain
      loop (n-1) a' 

    force' arr | sync = forcePull arr
               | otherwise = unsafeWritePull False arr

mapNonsense :: (Storable a, Num a)  => Bool -> Word32 -> Word32 -> DPull a -> DPush Grid a
mapNonsense sync blocksize seq_depth arr = pConcat $ fmap sConcat (fmap (fmap body) arr')
  where
    body = runPush . (nonsense  sync) 
    arr' =  fmap (splitUp (blocksize `div` seq_depth)) (splitUp blocksize arr)

-- ######################################################################
-- Sklansky Kernel 
-- ######################################################################
sklansky :: (Choice a, Storable a)
            => Int
            -> (a -> a -> a)
            -> Pull Word32 a
            -> BProgram (Push Block Word32 a)
sklansky 0 op arr = return (push arr)
sklansky n op arr =
  do 
    let arr1 = binSplit (n-1) (fan op) arr
    arr2 <- forcePull arr1
    sklansky (n-1) op arr2


fan :: Choice a
       => (a -> a -> a)
       -> SPull a
       -> SPull a
fan op arr =  a1 `append` fmap (op c) a2 
    where 
      (a1,a2) = halve arr
      c = a1 ! fromIntegral (len a1 - 1)

-- pushM = liftM push

mapScan1 :: (Choice a, Storable a) => Word32 -> Word32 -> Int -> (a -> a -> a) -> DPull a -> DPush Grid a
mapScan1 blocksize seq_depth n f arr = pConcat $ fmap sConcat (fmap (fmap body) arr') 
  where
    body arr = runPush (sklansky n f arr)
    arr' =  fmap (splitUp (blocksize `div` seq_depth)) (splitUp blocksize arr)




kernels :: [(String, (Word32,Word32 -> Word32 -> DPull EWord32 -> DPush Grid EWord32))]
kernels = [("REDUCTION", (16384,\b s -> mapRed3 b s (+)))
          ,("MAPCHAIN_NOSYNC", (8388608, (mapNonsense False)))
          ,("MAPCHAIN_SYNC"  , (8388608, (mapNonsense True)))
          ,("SKLANSKY", (8388608,\b s -> mapScan1 b s 9 (+)))]
          


-- ######################################################################
-- Main
-- ######################################################################

-- these remain constant over benchspace
data_size = 8388608
--results_size = 16384 

-- This varies (this is the benchspace) 
-- grid_size -> (elementsperblock,seq depth mapping)
grid_sizes = [(16384,(512,1))
             ,(8192 ,(1024,2))
             ,(4096 ,(2048,4))
             ,(2048 ,(4096,8))
             ,(1024 ,(8192,16))
             ,(512  ,(16384,32))
             ,(256  ,(32768,64))
             ,(128  ,(65536,128))
             ,(64   ,(131072,256))
             ,(32   ,(262144,512))
             ,(16   ,(524288,1024))
             ,(8    ,(1048576,2048))
             ,(4    ,(2097152,4096))
             ,(2    ,(4194393,8192))
             ,(1    ,(8388608,16384))]


exitError = do
  error $ "Provide 2 args: Benchmark [\"REDUCTION\",\"NONSENSE\"] and Grid size (one of: "  ++ show (map fst grid_sizes) ++ ")"

main :: IO ()
main = do
  putStrLn "Running GridSize benchmark.."
  args <- getArgs
  case length args of
    2 -> do
      let g_size = read $ args P.!! 1
      case P.lookup g_size grid_sizes of
        Nothing -> exitError 
        Just seq_depth ->
          case P.lookup (args P.!! 0) kernels of
            Just k -> performBench k g_size seq_depth 
            Nothing -> exitError
    _ -> exitError 

  where
    performBench (results_size,k) g_size (blocksize,seq_depth) = 
      withCUDA $
      do lift $ putStrLn $ "  grid: " P.++ show g_size P.++ "\n  elt/block: " P.++ show blocksize P.++
                           "\n  seq_depth: " P.++ show seq_depth
          
         compile_t0 <- lift getCurrentTime 
         capt <-
           capture 512 (k blocksize seq_depth) -- (mapRed3 blocksize seq_depth (+))
         compile_t1 <- lift getCurrentTime
           
         --(inputs :: V.Vector Word32) <- lift $ mkRandomVec (fromIntegral (data_size))
         let (inputs :: V.Vector Word32) = V.fromList (P.replicate data_size 1 )

         _ <- inputs `deepseq` return () 
         
         transfer_start <- lift getCurrentTime
         useVector inputs $ \i ->
           allocaVector (fromIntegral results_size)  $ \(o :: CUDAVector Word32) ->
           do transfer_done <- lift getCurrentTime 

              fill o 0

              t0 <- lift getCurrentTime
              cnt0 <- lift rdtsc
              forM_ [0..999::Int] $ \_ -> do
                o <== (g_size,capt) <> i
                syncAll
              cnt1 <- lift rdtsc
              t1 <- lift getCurrentTime

              --r <- peekCUDAVector o
              r <- copyOut o 
              t_end <- lift getCurrentTime
              
              lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
              lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)

              lift $ putStrLn $ "COMPILATION_TIME: " ++ show (diffUTCTime compile_t1 compile_t0)
    
              lift $ putStrLn $ "BYTES_TO_DEVICE: " ++ show (data_size * sizeOf (undefined :: EWord32))
              lift $ putStrLn $ "BYTES_FROM_DEVICE: " ++ show (fromIntegral results_size * sizeOf (undefined :: EWord32))
              lift $ putStrLn $ "TRANSFER_TO_DEVICE: " ++ show (diffUTCTime transfer_done transfer_start)
              lift $ putStrLn $ "TRANSFER_FROM_DEVICE: " ++ show (diffUTCTime t_end t1)
              lift $ putStrLn $ show $ P.take 1024 (V.toList r) 



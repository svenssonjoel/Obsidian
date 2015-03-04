
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Scan
-- for Large scans import Reduce.
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


import System.Environment
import System.CPUTime.Rdtsc

import Data.Time.Clock
import Control.DeepSeq 

-- ######################################################################
-- Tools
-- ######################################################################
imLog :: Integer->Integer->Integer
imLog b x
  = if x < b
    then 0
    else
      let
        l = 2 * imLog (b*b) x
        doDiv x l = if x < b then l else doDiv (x`div`b) (l+1)
      in
       doDiv (x`div`(b^l)) l

segmentedScan segSize vec | V.length vec == segSize = V.scanl1 (+) vec
                          | V.length vec < segSize = error "Incorrect vector size"
segmentedScan segSize vec = V.scanl1 (+) (V.take segSize vec) V.++
                            segmentedScan segSize (V.drop segSize vec) 



-- ######################################################################
-- Kernels
-- ######################################################################
kernels = [("chain1",seqChain sklanskies)
          ,("chain2",seqChain kss)
          ,("chain3",seqChain ksps)]

       
-- ######################################################################
-- Main
-- ######################################################################
main = do

  args <- getArgs
  -- n (not (length args == 3 || length args == 5) $
    
  case length args of 
    4 -> do 
      let k = args P.!! 0

          num_blocks = read $ args P.!! 1
          elts_per_block = read $ args P.!! 2
--          total_elts = num_blocks * elts_per_block
          threads = read $ args P.!! 3          
      small num_blocks elts_per_block threads k
    5 -> do 
      let iscan     = args P.!! 0
          scan      = args P.!! 1 
          blocks    = read $ args P.!! 2
          elements  = read $ args P.!! 3
          threads   = read $ args P.!! 4
      large iscan scan blocks elements threads
    _ -> error "Wrong arguments" 
  

small num_blocks elts_per_block threads k = do 

  putStrLn "Running BLOCK scan benchmark..."
  
  let eLog = fromIntegral $ imLog 2 (fromIntegral threads)
      
  withCUDA $ do

    compile_t0 <- lift getCurrentTime 
    capt <- case (lookup k kernels) of
      Nothing -> error "Incorrect kernel"
      Just kern -> capture threads (\accs arr -> kern eLog (+) accs (splitUp elts_per_block arr))
    
    compile_t1 <- lift getCurrentTime

    (inputs :: V.Vector Word32) <- lift $ mkRandomVec (fromIntegral (elts_per_block * num_blocks))
    let cpuresult = segmentedScan (fromIntegral elts_per_block) (V.map (`mod` 32) inputs)
          -- V.scanl1 (+) (V.map (`mod` 32) inputs)

    transfer_start <- lift getCurrentTime
    useVector (V.map (`mod` 32) inputs) $ \i -> -- (V.fromList (P.replicate (fromIntegral e) 1))  $ \i ->
      allocaVector (fromIntegral num_blocks) $ \ (carry_ins :: CUDAVector Word32) -> 
        allocaVector (fromIntegral (elts_per_block*num_blocks))  $ \(o :: CUDAVector Word32) -> do
          transfer_done <- lift getCurrentTime
          fill o 0
          fill carry_ins 0 

          t0   <- lift getCurrentTime
          cnt0 <- lift rdtsc
          forM_ [0..999] $ \_ -> do
            o <== (num_blocks,capt) <> carry_ins <> i
            syncAll
          cnt1 <- lift rdtsc
          t1   <- lift getCurrentTime


        
          r <- copyOut o
          t_end <- lift getCurrentTime
        
  
          lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
          lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)

          lift $ putStrLn $ "COMPILATION_TIME: " ++ show (diffUTCTime compile_t1 compile_t0)
    
          lift $ putStrLn $ "BYTES_TO_DEVICE: " ++ show (fromIntegral (elts_per_block * num_blocks) * sizeOf (undefined :: EWord32))
          lift $ putStrLn $ "BYTES_FROM_DEVICE: " ++ show (fromIntegral (elts_per_block * num_blocks) * sizeOf (undefined :: EWord32))
          lift $ putStrLn $ "TRANSFER_TO_DEVICE: " ++ show (diffUTCTime transfer_done transfer_start)
          lift $ putStrLn $ "TRANSFER_FROM_DEVICE: " ++ show (diffUTCTime t_end t1)
          lift $ putStrLn $ "NUMBER_OF_BLOCKS: " ++ show num_blocks
          lift $ putStrLn $ "ELEMENTS_PER_BLOCK: " ++ show elts_per_block 
          lift $ putStrLn $ "ELEMENTS_PROCESSED: " ++ show (num_blocks * elts_per_block)

          lift $ putStrLn "Done: ... Comparing to CPU result"         
          case (r == cpuresult) of
            False -> lift $ putStrLn "WARNING: GPU and CPU results don't match "
            True -> lift $ putStrLn "GREAT! GPU and CPU results match!"
           



-- ---------------------------------------------------------------------------
-- -- Perform large scan benches
-- ---------------------------------------------------------------------------

iScanKernels :: [(String,
                        Int
                        -> (Exp Word32 -> Exp Word32 -> Exp Word32) -> Exp Word32 -> DPull (SPull (Exp Word32)) -> DPush Grid (Exp Word32))]
iScanKernels = [("iscan1", mapIScan1)
               ,("iscan2", mapIScan2)
               ,("iscan3", mapIScan3)
               ,("iscan4", mapIScan4)
               ,("iscan5", mapIScan5)                
               ] 

large iscan scan blocks elements threads = do 
  putStrLn "Running LARGE scan benchmark..."
  let sk = scan  
      isk = iscan
      --blocks   
      t = threads 
      e = elements 
  
  let eLog = fromIntegral $ imLog 2 (fromIntegral threads)
      blocksLog = fromIntegral $ imLog 2 (fromIntegral blocks)
  
  withCUDA $ do
    compile_t0 <- lift getCurrentTime 

    --captRed <- case (lookup rk reductionKernels) of 
    --  Nothing -> error $ "incorrect reduce kernel: " ++ rk ++ " " ++ show (map fst reductionKernels)  
    --Just kern -> capture t (kern . splitUp e)
    lift $ putStrLn "capture reduce"
    captRed <- capture t ((mapRed8 t (+)) . splitUp e)

    lift $ putStrLn "capture scan"
    captScan <- case (lookup sk kernels) of
      Nothing -> error $ "incorrect scan kernel: " ++ sk ++ " " ++ show (map fst kernels)
      Just kern -> capture t (\a b -> kern eLog (+) a (splitUp e b))
  
    -- Needs to be splitting up in blcks batches
    -- and use t threads
    lift $ putStrLn "capture iscan"
    captIScan <- case (lookup isk iScanKernels) of
      Nothing -> error $ "incorrect iscan kernel: " ++ isk ++ " " ++ show (map fst iScanKernels)
      Just kern -> capture t (\a b -> kern blocksLog (+) a (splitUp (fromIntegral blocks) b))
    compile_t1 <- lift getCurrentTime  

    --(inputs :: V.Vector Word32) <- lift $ mkRandomVec (fromIntegral (e * blocks))
    let inputs = V.fromList $ P.take (fromIntegral (e*blocks)) (P.repeat 1)
        cpuresult = V.scanl1 (+) inputs 

    transfer_start <- lift getCurrentTime
    useVector inputs $ \i ->
      allocaVector (fromIntegral blocks) $ \ (reds :: CUDAVector Word32) ->
        allocaVector (fromIntegral (e * blocks)) $ \ (o :: CUDAVector Word32) ->
          -- allocaVector 1 $ \ (zero :: CUDAVector Word32) ->
        
          do
            transfer_done <- lift getCurrentTime
            
            t0   <- lift getCurrentTime
            cnt0 <- lift rdtsc

            forM_ [0..999] $ \_ -> do
              -- fill zero 0 
              reds <== (blocks,captRed) <> i
              reds <== (1,captIScan) <> (0 :: Word32) <> reds
              o <== (blocks,captScan) <> reds <> i
              syncAll
            
            cnt1 <- lift rdtsc
            t1   <- lift getCurrentTime


            r <- copyOut o
            t_end <- lift getCurrentTime
            

            lift $ putStrLn $ "ELEMENTS_PROCESSED: " ++ show ( fromIntegral (blocks * e))
            lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
            lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)

            
            lift $ putStrLn $ "COMPILATION_TIME: " ++ show (diffUTCTime compile_t1 compile_t0)
    
            lift $ putStrLn $ "BYTES_TO_DEVICE: " ++ show (fromIntegral (e * blocks) * sizeOf (undefined :: EWord32))
            lift $ putStrLn $ "BYTES_FROM_DEVICE: " ++ show (fromIntegral (e * blocks) * sizeOf (undefined :: EWord32))
            lift $ putStrLn $ "TRANSFER_TO_DEVICE: " ++ show (diffUTCTime transfer_done transfer_start)
            lift $ putStrLn $ "TRANSFER_FROM_DEVICE: " ++ show (diffUTCTime t_end t1)

            lift $ putStrLn $ "ELEMENTS_PROCESSED: " ++ show (fromIntegral (e * blocks))
            lift $ putStrLn $ "NUMBER_OF_BLOCKS: "   ++ show (fromIntegral blocks)
            lift $ putStrLn $ "ELEMENTS_PER_BLOCK: " ++ show (fromIntegral e)
            lift $ putStrLn $ show $ P.take 10 (V.toList r)

            lift $ putStrLn "Done: ... Comparing to CPU result"         
            case (r == cpuresult) of
              False ->
                do lift $ putStrLn "WARNING: GPU and CPU results don't match "
                   lift $ putStrLn $ show $ P.zip (V.toList cpuresult) (V.toList r )
                   --lift $ putStrLn $ show $ r_reds
                   --lift $ putStrLn $ show $ r_reds2
              True -> lift $ putStrLn "GREAT! GPU and CPU results match!"


  

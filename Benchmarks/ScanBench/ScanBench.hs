
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
kernels = [("s1", mapScan1)
          ,("s2", mapScan2)
          ,("s3", mapScan3)
          ,("k1", mapScan4)
          ,("k2", mapScan5)]
          -- ,("chain1",seqChain sklanskies 0)
          -- ,("chain2",seqChain kss 0)
          -- ,("chain3",seqChain ksps 0)]

       
-- ######################################################################
-- Main
-- ######################################################################
main = do

  args <- getArgs
  -- n (not (length args == 3 || length args == 5) $
    
  case length args of 
    3 -> do 
      let k = args P.!! 0 
          t = read $ args P.!! 1
          e = read $ args P.!! 2
          blcks = 8192
      small k t e 
    5 -> do
      let k1 = args P.!! 1
          k2 = args P.!! 2
          t  = read $ args P.!! 3
          e  = read $ args P.!! 4
      large k1 k2 t e 
    _ -> error "Wrong arguments" 
  


  
small k t e = do 

  putStrLn "Running BLOCK scan benchmark..."
  
  let eLog = fromIntegral $ imLog 2 (fromIntegral e) 
      blcks = 8192 
  withCUDA $ do

    compile_t0 <- lift getCurrentTime 
    capt <- case (lookup k kernels) of
      Nothing -> error "Incorrect kernel"
      Just kern -> capture t (kern eLog (+) . splitUp e)
    
    compile_t1 <- lift getCurrentTime

    (inputs :: V.Vector Word32) <- lift $ mkRandomVec (fromIntegral (e * blcks))
    let cpuresult = segmentedScan (fromIntegral e) (V.map (`mod` 32) inputs)
          -- V.scanl1 (+) (V.map (`mod` 32) inputs)

    transfer_start <- lift getCurrentTime
    useVector (V.map (`mod` 32) inputs) $ \i -> -- (V.fromList (P.replicate (fromIntegral e) 1))  $ \i ->
      allocaVector (fromIntegral (e*blcks))  $ \(o :: CUDAVector Word32) -> do
        transfer_done <- lift getCurrentTime
        fill o 0


        t0   <- lift getCurrentTime
        cnt0 <- lift rdtsc
        forM_ [0..999] $ \_ -> do
          o <== (blcks,capt) <> i
          syncAll
        cnt1 <- lift rdtsc
        t1   <- lift getCurrentTime


        
        r <- copyOut o
        t_end <- lift getCurrentTime
        
        --lift $ putStrLn $ show r
        --lift $ putStrLn $ show cpuresult
        --lift $ putStrLn $ show (r == cpuresult)

        -- Computing the cpuresults take a long time!
        -- I implemented a bad cpu segmented scan (uses V.++)         
        lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
        lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)

        lift $ putStrLn $ "COMPILATION_TIME: " ++ show (diffUTCTime compile_t1 compile_t0)
    
        lift $ putStrLn $ "BYTES_TO_DEVICE: " ++ show (fromIntegral (e * blcks) * sizeOf (undefined :: EWord32))
        lift $ putStrLn $ "BYTES_FROM_DEVICE: " ++ show (fromIntegral (e * blcks) * sizeOf (undefined :: EWord32))
        lift $ putStrLn $ "TRANSFER_TO_DEVICE: " ++ show (diffUTCTime transfer_done transfer_start)
        lift $ putStrLn $ "TRANSFER_FROM_DEVICE: " ++ show (diffUTCTime t_end t1)

        lift $ putStrLn $ "ELEMENTS_PROCESSED: " ++ show (fromIntegral (e * blcks))
        lift $ putStrLn $ "NUMBER_OF_BLOCKS: "   ++ show (fromIntegral blcks)
        lift $ putStrLn $ "ELEMENTS_PER_BLOCK: " ++ show (fromIntegral e) 
        
        lift $ putStrLn $ show $ P.take 10 (V.toList r) 

        -- lift $ putStrLn "Done: ... Comparing to CPU result"
        -- case r == cpuresult of
        --   False -> lift $ putStrLn "WARNING: GPU and CPU results don't match!"
        --   True  -> lift $ putStrLn "GREAT! GPU and CPU results are equal."



---------------------------------------------------------------------------
-- Perform large scan benches
---------------------------------------------------------------------------

        -- RENAME cin to actuall scan kernel name s1 - s3 k1,k2 
reductionKernels :: [(String, DPull (SPull (Exp Word32)) -> DPush Grid (Exp Word32))]
reductionKernels = [("rbs_1", mapRed1 (+))
                   ,("rbs_2", mapRed2 (+))
                   ,("rbs_3", mapRed3 (+))
                   ,("rbs_4", mapRed4 (+))
                   ,("rbs_5", mapRed5 (+))
                   ,("rbs_6", mapRed6 (+))
                   ,("rbs_7", mapRed7 (+))]
scanKernels :: [(String,
                       Int
                       -> (Exp Word32 -> Exp Word32 -> Exp Word32)
                       -> DPull (Exp Word32)
                       -> DPull (SPull (Exp Word32))
                       -> DPush Grid (Exp Word32))]
scanKernels = [("cin1", mapScanCIn1)
              ,("cin2", mapScanCIn2)
              ,("cin3", mapScanCIn3)
              ,("cin4", mapScanCIn4)
              ,("cin5", mapScanCIn5)] 

-- iscan kernels is the iscan version matching the cin kernel
-- hence same identifier
iScanKernels :: [(String,
                        Int
                        -> (Exp Word32 -> Exp Word32 -> Exp Word32) -> Exp Word32 -> DPull (SPull (Exp Word32)) -> DPush Grid (Exp Word32))]
iScanKernels = [("cin1", mapIScan1)
               ,("cin2", mapIScan2)
               ,("cin3", mapIScan3)
               ,("cin4", mapIScan4)
               ,("cin5", mapIScan5)                
               ] 

large reducer scan threads elements = do 
  putStrLn "Running LARGE scan benchmark..."
  let rk = reducer -- args P.!! 0
      sk = scan 
      t = threads -- read $ args P.!! 1
      e = elements -- read $ args P.!! 2
      blcks = 4096
  -- blcks
  -- The number of blocks sets the size of the innermost scan.
  -- We can vary number of threads in the innermost scan as t.
  -- But number of elements needs to be fixed at blcks 
  
  let eLog = fromIntegral $ imLog 2 (fromIntegral e)
      blocksLog = fromIntegral $ imLog 2 (fromIntegral blcks)
  
  withCUDA $ do
    compile_t0 <- lift getCurrentTime 
    captRed <- case (lookup rk reductionKernels) of 
      Nothing -> error "incorrect reduce kernel" 
      Just kern -> capture t (kern . splitUp e)

    captScan <- case (lookup sk scanKernels) of
      Nothing -> error "incorrect scan kernel"
      Just kern -> capture t (\a b -> kern eLog (+) a (splitUp e b))

    -- Needs to be splitting up in blcks batches
    -- and use t threads 
    captIScan <- case (lookup sk iScanKernels) of
      Nothing -> error "incorrect scan kernel"
      Just kern -> capture t (\a b -> kern blocksLog (+) a (splitUp (fromIntegral blcks) b))
    compile_t1 <- lift getCurrentTime  

    --(inputs :: V.Vector Word32) <- lift $ mkRandomVec (fromIntegral (e * blcks))
    let inputs = V.fromList $ P.take (fromIntegral (e*blcks)) (P.repeat 1)
        cpuresult = V.scanl1 (+) inputs 

    transfer_start <- lift getCurrentTime
    useVector inputs $ \i ->
      allocaVector (fromIntegral blcks) $ \ (reds :: CUDAVector Word32) ->
        allocaVector (fromIntegral (e * blcks)) $ \ (o :: CUDAVector Word32) ->
          allocaVector 1 $ \ (zero :: CUDAVector Word32) ->
        
          do
            transfer_done <- lift getCurrentTime
            
            t0   <- lift getCurrentTime
            cnt0 <- lift rdtsc

            forM_ [0..999] $ \_ -> do
              fill zero 0 
              reds <== (t,captRed) <> i
              reds <== (1,captIScan) <> (0 :: Word32) <> reds
              o <== (t,captScan) <> reds <> i
              syncAll
            
            cnt1 <- lift rdtsc
            t1   <- lift getCurrentTime


            r <- copyOut o 
            t_end <- lift getCurrentTime
            

            lift $ putStrLn $ "ELEMENTS_PROCESSED: " ++ show ( fromIntegral (blcks * e))
            lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
            lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)

            
            lift $ putStrLn $ "COMPILATION_TIME: " ++ show (diffUTCTime compile_t1 compile_t0)
    
            lift $ putStrLn $ "BYTES_TO_DEVICE: " ++ show (fromIntegral (e * blcks) * sizeOf (undefined :: EWord32))
            lift $ putStrLn $ "BYTES_FROM_DEVICE: " ++ show (fromIntegral (e * blcks) * sizeOf (undefined :: EWord32))
            lift $ putStrLn $ "TRANSFER_TO_DEVICE: " ++ show (diffUTCTime transfer_done transfer_start)
            lift $ putStrLn $ "TRANSFER_FROM_DEVICE: " ++ show (diffUTCTime t_end t1)
            lift $ putStrLn $ "ELEMENTS_PROCESSED: " ++ show (fromIntegral (e * blcks))
            lift $ putStrLn $ "NUMBER_OF_BLOCKS: "   ++ show (fromIntegral blcks)
            lift $ putStrLn $ "ELEMENTS_PER_BLOCK: " ++ show (fromIntegral e) 
        
--        lift $ putStrLn $ show $ P.take 10 (V.toList r) 

            lift $ putStrLn "Done: ... Comparing to CPU result"
            case r == cpuresult of
              False -> lift $ putStrLn "WARNING: GPU and CPU results don't match!"
              True  -> lift $ putStrLn "GREAT! GPU and CPU results are equal."



  


module Main where

import Control.Monad
import Data.Functor
import System.Environment
import System.Exit

import Obsidian as O
import Obsidian.Run.CUDA.Exec (withCUDA, capture)

buildChain :: Num a => Int -> Pull s a -> Pull s a 
buildChain 0 arr = arr
buildChain i arr = fmap (+1) $ buildChain (i-1) arr

main :: IO ()
main = do 
  putStrLn "Running MapLadder benchmark... this involves a series of Maps where the results are used more than once."
  args <- getArgs
  (len) <- case args of 
             []  -> do putStrLn "Using default settings!  Provide command line args: "
                       return 10
             [l] -> return (read l :: Int)
--  when (length args > 3 || length args < 3) $
  print len
--  runBenchmark kern t e
  runBenchmark len


mapRed1 :: Storable a => (a -> a -> a) -> DPull (SPull a) -> DPush Grid a
mapRed1 f arr = pConcat (fmap body arr)
  where
    body arr = singletonPush (red1 f arr) 

red1 :: Storable a
      => (a -> a -> a)
      -> SPull a
      -> BProgram a
red1 f arr
  | len arr == 1 = return (arr ! 0)
  | otherwise    = 
    do
      let (a1,a2) = evenOdds arr
      imm <- forcePull $ O.zipWith f a1 a2
      red1 f imm   


-- runBenchmark :: (Pull EWord32 (SPull (Exp Word32)) -> DPush Grid (Exp Word32)) -> Word32 -> Word32 -> IO ()
-- runBenchmark kern t elts =
runBenchmark len =
  withCUDA $
  do
    capt <- capture t (kern . splitUp elts)
    undefined    

--     (inputs :: V.Vector Word32) <- lift $ mkRandomVec (fromIntegral (blcks * elts))
    
--     let cpuresult = V.sum inputs 
    
--     useVector inputs $ \i ->
--       allocaVector (fromIntegral blcks)  $ \(o :: CUDAVector Word32) ->
--         body cpuresult capt i o
--         --allocaVector 1  $ \(o2 :: CUDAVector Word32) -> do 
                                                        
--   where
--     blcks = 8192
--     body cpuresult kern i o = 
--         do
--           fill o 0
        

--           t0   <- lift getCurrentTime
--           cnt0 <- lift rdtsc
--           forM_ [0..999] $ \_ -> do 
--             o <== (blcks,kern) <> i
--             syncAll
--           cnt1 <- lift rdtsc
--           t1   <- lift getCurrentTime

--           r <- peekCUDAVector o
--           when (sum r /= cpuresult) $ lift $ exitWith (ExitFailure 1) 


--           lift $ putStrLn $ "SELFTIMED: " ++ show (diffUTCTime t1 t0)
--           lift $ putStrLn $ "CYCLES: "    ++ show (cnt1 - cnt0)

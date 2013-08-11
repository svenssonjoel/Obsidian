
module Scan   where

import Obsidian

import Prelude hiding (take, drop) 

sklanskyLocal
  :: (Choice a, MemoryOps a) =>
     Int
     -> (a -> a -> a)
     -> SPull a
     -> BProgram (SPush Block a)
sklanskyLocal 0 op arr = return $ push arr
sklanskyLocal n op arr =
  do 
    let arr1 = binSplit (n-1) (fan op) arr
    arr2 <- force arr1
    sklanskyLocal (n-1) op arr2

sklansky n op = pConcatMap $ pJoin . (sklanskyLocal n op)

fan op arr =  a1 `conc`  fmap (op c) a2 
    where 
      (a1,a2) = halve arr
      c = a1 ! (fromIntegral (len a1 - 1))              

getScan = putStrLn $ genKernel "scan" (sklansky 8 (+) . splitUp 256) (input :- ())
   where
     input :: DPull EWord32
     input = undefinedGlobal (variable "X")

  

sklanskyLocalCin
  :: (Choice a, MemoryOps a) =>
     Int
     -> (a -> a -> a)
     -> a 
     -> SPull a
     -> BProgram (SPush Block a)
sklanskyLocalCin n op cin arr =
  do
    arr' <- force $ applyToHead op cin arr 

    sklanskyLocal n op arr'
  where
    applyToHead op cin arr =
      let h = fmap (op cin) $ take 1 arr
          b = drop 1 arr
      in h `conc` b

--sklanskyCin n op = pConcatMap $ pJoin . (sklanskyLocal n op)

--getScanCin = putStrLn $ genKernel "scan" (sklanskyCin 8 (+) . splitUp 256) (input :- ())
--   where
--     input :: DPull EWord32
--     input = undefinedGlobal (variable "X")


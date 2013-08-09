
module Scan   where

import Obsidian

import Prelude

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

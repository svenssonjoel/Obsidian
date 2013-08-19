
module Scan   where

import Obsidian

import Prelude hiding (take, drop, zipWith)
import Control.Applicative

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

sklanskyLocalInc n op a arr =
  do
    arr' <- force =<<  sklanskyLocal n op arr
    let arr'' = take (len arr) $ singleton a `conc` arr'
    return $ push arr''    

sklanskyInc n op a  = pConcatMap $ pJoin . (sklanskyLocalInc n op a)    

fan op arr =  a1 `conc`  fmap (op c) a2 
    where 
      (a1,a2) = halve arr
      c = a1 ! (fromIntegral (len a1 - 1))              

-- getScan = putStrLn $ genKernel "scan" (sklansky 8 (+) . splitUp 256) (input :- ())
--    where
--      input :: DPull EWord32
--      input = undefinedGlobal (variable "X")

  

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
      let h = fmap (op cin ) $ take 1 arr
          b = drop 1 arr
      in h `conc` b

sklanskyCin n op cins arr
  = pConcat $
    zipWith (\a b -> pJoin $ sklanskyLocalCin n op a b) cins arr

-- getScanCin =
--   putStrLn $ genKernel "scan" kernel (inputC :- input :- ())
--    where
--      kernel cins arr = sklanskyCin 8 (+) cins (splitUp 256 arr) 
--      input :: DPull EWord32
--      input = undefinedGlobal (variable "X")

--      inputC :: DPull EWord32
--      inputC = undefinedGlobal (variable "X")
     


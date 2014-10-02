{-# LANGUAGE GADTs #-} 
module Scan where

import Obsidian

import Prelude hiding (take, drop, zipWith)
import Data.Word

---------------------------------------------------------------------------
-- Sklansky Scan
--------------------------------------------------------------------------- 
sklanskyLocal
  :: (Choice a, Storable a) =>
     Int
     -> (a -> a -> a)
     -> SPull a
     -> BProgram (SPush Block a)
sklanskyLocal 0 op arr = return $ push arr
sklanskyLocal n op arr =
  do 
    let arr1 = binSplit (n-1) (fan op) arr
    arr2 <- force $ push arr1
    sklanskyLocal (n-1) op arr2

fan op arr =  a1 `append`  fmap (op c) a2 
    where 
      (a1,a2) = halve arr
      c = a1 ! (fromIntegral (len a1 - 1))              


sklansky n op arr = pConcatMap body arr
  where
    body arr = runPush (sklanskyLocal n op arr)





---------------------------------------------------------------------------
--Kogge-Stone
--------------------------------------------------------------------------- 
-- 0th element is done,
-- stage i: combine element j with element 2^i away:
-- stage 0: combine j with j-1 (j > 0) 
-- stage 1: combine j with j-2 (j > 1)
-- stage 2: combine j wiht j-4 (j > 3) 
ksLocal :: (Choice a, Storable a) 
        => Int -- Stages (log n) 
        -> (a -> a -> a) -- opertor
        -> SPull a -- input data
        -> BProgram (SPush Block a) -- output computation
ksLocal 0 op arr = return $ push arr
ksLocal n op arr = do
  arr2 <- force =<< ksLocal (n-1) op arr
  let a1   = shiftLeft (2^(n-1)) arr2
      oped = zipWith op arr2 a1 
      copy = take (2^(n-1)) arr2
      all  = copy `append` oped
  return $push all
-- Little bit ugly... 
  
             
ks n op arr = pConcatMap body arr
  where
    body arr = runPush (ksLocal n op arr)
                   

    
        
           

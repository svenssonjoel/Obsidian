
module Scan where

import Obsidian

import Prelude hiding (take, drop, zipWith)


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



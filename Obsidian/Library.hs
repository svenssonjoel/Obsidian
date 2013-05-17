{- Joel Svensson 2012, 2013 
   Mary Sheeran  2012

   Notes:
   2013-01-24: GlobPull nolonger exists
               GlobPush is Push Grid
              
   2013-01-08: Renamed GlobArray to GlobPush 
   2013-01-02: Added toGlobArray and toGlobArrayN
   2012-12-10: Refactoring
               (adherence to new Array types and program types)  
-}

{-# LANGUAGE FlexibleInstances,
             TypeSynonymInstances,
             ScopedTypeVariables,
             TypeFamilies,
             GADTs #-}

module Obsidian.Library where 

import Obsidian.Array 
import Obsidian.Exp 
import Obsidian.Program
import Obsidian.Types

import Obsidian.Force

-- needed for threadsPerBlock analysis 
import qualified Obsidian.CodeGen.Program as P 

import Data.Bits 
import Data.Word

import Prelude hiding (splitAt,zipWith,replicate,reverse)




---------------------------------------------------------------------------
-- Reverse an array by indexing in it backwards
---------------------------------------------------------------------------
  
reverse :: ASize l => Pull l a -> Pull l a 
reverse arr = mkPullArray n (\ix -> arr ! ((sizeConv m) - ix))  
   where m = n-1
         n = len arr
         
---------------------------------------------------------------------------
-- splitAt (name clashes with Prelude.splitAt)
---------------------------------------------------------------------------
splitAt :: (Integral i, ASize l) => i -> Pull l a -> (Pull l a, Pull l a) 
splitAt n arr = (mkPullArray m (\ix -> arr ! ix), 
                 mkPullArray  (len arr - m) (\ix -> arr ! (ix + pos)))
  where pos = fromIntegral n
        m   = fromIntegral n


halve arr = splitAt n2 arr
  where 
    n = len arr
    n2 = n `div` 2

---------------------------------------------------------------------------
-- replicate 
---------------------------------------------------------------------------
replicate n a = mkPullArray n (\ix -> a)

singleton a = replicate 1 a 

---------------------------------------------------------------------------
-- last
---------------------------------------------------------------------------

last arr = arr ! fromIntegral ( len arr - 1)


---------------------------------------------------------------------------
-- Take and Drop (what about strange sizes ? fix) 
---------------------------------------------------------------------------
take :: ASize l => l -> Pull l a -> Pull l a
take n arr = resize n arr

drop :: ASize l => l -> Pull l a -> Pull l a
drop n arr = resize (len arr - n) $ ixMap (\ix -> ix + sizeConv n) arr

---------------------------------------------------------------------------
-- Shift arrays
---------------------------------------------------------------------------
shiftRight :: (ASize l, Choice a) => Word32 -> a -> Pull l a -> Pull l a
shiftRight dist elt arr = resize (len arr)
                          $ replicate (fromIntegral dist) elt `conc` arr

-- TODO: incorrect! 
shiftLeft :: (ASize l, Choice a) => Word32 -> a -> Pull l a -> Pull l a
shiftLeft dist elt arr = mkPullArray (len arr)
                         $ \ix -> (arr `conc`  replicate (fromIntegral dist) elt)
                                  ! (ix + fromIntegral dist) 
                         
---------------------------------------------------------------------------
-- elements at even indices to fst output, odd to snd.
---------------------------------------------------------------------------
evenOdds :: ASize l => Pull l a -> (Pull l a, Pull l a)
evenOdds arr = (mkPullArray (n-n2) (\ix -> arr ! (2*ix)) ,
                mkPullArray n2     (\ix -> arr ! (2*ix + 1)))
  where
    n  = len arr
    n2 = div n 2
    
evens, odds :: ASize l => Pull l a -> Pull l a
evens = fst . evenOdds
odds  = snd . evenOdds

-- opposite of evenOdds 
--shuffle :: ASize l => PT t -> Pull l a -> Pull l a -> Push t l a
--shuffle Block a1 a2 =
--  Push (len a1 + len a2) $
--    \ wf -> ForAll (sizeConv (len a1)) $
--            \ tid -> do
--              wf (a1 ! tid) (tid * 2) 
--              wf (a2 ! tid) (tid * 2 + 1) 


---------------------------------------------------------------------------
-- Concatenate the arrays
---------------------------------------------------------------------------
conc :: (ASize l, Choice a) => Pull l a -> Pull l a -> Pull l a 
conc a1 a2 = mkPullArray (n1+n2)
               $ \ix -> ifThenElse (ix <* (fromIntegral n1)) 
                       (a1 ! ix) 
                       (a2 ! (ix - (fromIntegral n1)))
  where 
    n1 = len a1
    n2 = len a2 

    
---------------------------------------------------------------------------
-- zipp unzipp
---------------------------------------------------------------------------
unzipp :: ASize l =>  Pull l (a,b) -> (Pull l a, Pull l b)       
unzipp arr = (mkPullArray (len arr) (\ix -> fst (arr ! ix)) ,
              mkPullArray (len arr) (\ix -> snd (arr ! ix)) )
              
zipp :: ASize l => (Pull l a, Pull l b) -> Pull l (a, b)             
zipp (arr1,arr2) =  Pull (min (len arr1) (len arr2))
                      $ \ix -> (arr1 ! ix, arr2 ! ix) 

unzipp3 :: ASize l => Pull l (a,b,c) 
           -> (Pull l a, Pull l b, Pull l c)       
unzipp3 arr = (fmap (\(x,_,_) -> x) arr,
               fmap (\(_,y,_) -> y) arr,
               fmap (\(_,_,z) -> z)  arr) 


zipp3 :: ASize l =>  (Pull l a, Pull l b, Pull l c) 
         -> Pull l (a,b,c)             
zipp3 (arr1,arr2,arr3) = 
  mkPullArray (minimum [len arr1, len arr2, len arr3])
  (\ix -> (arr1 ! ix, arr2 ! ix, arr3 ! ix))
    

zipWith :: ASize l => (a -> b -> c) -> Pull l a -> Pull l b -> Pull l c
zipWith op a1 a2 =  
  mkPullArray (min (len a1) (len a2))
  (\ix -> (a1 ! ix) `op` (a2 ! ix))
                                      
---------------------------------------------------------------------------
-- pair 
---------------------------------------------------------------------------
pair :: ASize l => Pull l a -> Pull l (a,a)
pair (Pull n ixf) = 
  mkPullArray n' (\ix -> (ixf (ix*2),ixf (ix*2+1))) 
  where 
    n' = n `div` 2 



unpair :: ASize l => Choice a => Pull l (a,a) -> Pull l a
unpair arr = 
    let n = len arr
    in  mkPullArray (2*n) (\ix -> ifThenElse ((mod ix 2) ==* 0) 
                                  (fst (arr ! (ix `shiftR` 1)))
                                  (snd (arr ! (ix `shiftR` 1)))) 


---------------------------------------------------------------------------
-- twoK (untested for proper functionality) 
---------------------------------------------------------------------------

binSplit = twoK

-- See if this should be specifically for Static size pull arrays
twoK ::Int -> (Pull Word32 a -> Pull Word32 b) -> Pull Word32 a -> Pull Word32 b 
twoK 0 f = f  -- divide 0 times and apply f
twoK n f =  (\arr -> 
              let arr' = mkPullArray lt (\i -> (f (mkPullArray  m (\j -> (arr ! (g i j)))) ! (h i))) 
                  m    = (len arr `shiftR` n)   --pow of two           
                  g i j = i .&. (fromIntegral (complement (m-1))) .|. j  
                  h i   = i .&. (fromIntegral (nl2-1))   -- optimize 

                  nl2   = (len (f (mkPullArray  m (\j -> arr ! variable "X"))))
                  lt    = nl2 `shiftL` n 
              in arr')
 

---------------------------------------------------------------------------
-- ***                          PUSHY LIBRARY                       *** ---
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- Concatenate on Push arrays 
---------------------------------------------------------------------------
{-
concP :: (Array arr1, Array arr2, ASize l,
          Pushable arr1, Pushable arr2)
         => PT t -> arr1 l a -> arr2 l a -> Push t l a     
concP pt arr1 arr2 = 
  mkPushArray (n1 + n2)
  $ \wf ->
  do
    parr1 wf
    parr2 $ \a i -> wf a (sizeConv n1 + i)
  
  where
    n1 = len arr1 
    n2 = len arr2 
    (Push _ parr1) = push pt arr1
    (Push _ parr2) = push pt arr2
-}

-- More general versions of this can be imagined 
mergeL :: (EWord32 -> a -> a -> a) -> Pull Word32 a -> Pull Word32 a -> Push Block Word32 a
mergeL _ arr1 arr2 | len arr1 /= len arr2 = error "incorrect lengths" 
mergeL f arr1 arr2 =
  Push (len arr1) $ \wf ->
  do
    ForAll (sizeConv (len arr1)) $
      \ tid -> wf (f tid (arr1 ! tid) (arr2 ! tid)) tid 


---------------------------------------------------------------------------
-- Singleton push arrays 
---------------------------------------------------------------------------

singletonP a =
  Push 1 $ \wf ->
  do
    a' <- a
    wf a' 0 

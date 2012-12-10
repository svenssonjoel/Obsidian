{-# LANGUAGE TypeOperators, 
             GADTs, 
             FlexibleContexts #-} 
module Obsidian.GCDObsidian.AddOn.ArrowObsidian where 


import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Sync

import Control.Category
import Prelude hiding ((.),id)

import Data.Bits
import Data.Word

{- an attempt at implementing old ArrowObsidian as a 
   library on top of new "MonadObsidian" (GCDObsidian) -} 

data a :-> b where 
  Pure :: (a -> b) -> (a :-> b) 
  Sync :: Syncable Array b 
          => (a -> Array b) 
          -> (Array b :-> c) 
          -> (a :-> c) 
          

runArrow :: (a :-> b) -> a -> Kernel b 
runArrow (Pure f) a   = return$ f a
runArrow (Sync f g) a = 
  do 
    a' <- sync (f a) 
    runArrow g a'          
          
                    
instance Category (:->) where
  id = Pure id 
  
  (.) (Pure f) (Pure g) = Pure (f . g) 
  (.) (Pure f) (Sync h g) = Sync h (Pure f . g)
  (.) (Sync h g) (Pure f) = Sync (h . f) g 
          
                            
aSync :: Syncable Array a =>  Array a :-> Array a 
aSync = Sync id (Pure id)

----------------------------------------------------------------------------
-- Library
two :: (Array a :-> Array b) -> (Array a :-> Array b)
two (Pure f)  = Pure $ twoFF f
two (Sync f g) = Sync (twoFF f) (two g)

twoFF :: (Array a -> Array b) -> Array a -> Array b
twoFF f arr = 
    Array (\i -> f (
          Array (\j -> arr ! ((sh bit bit2 (i .&. num2)) .|. j)) n2) ! 
                                    (i .&. mask)) nl
    where 
      n2       = (len arr) `div` 2 :: Word32
      bit      = logInt n2 

      bit2     = logInt nl2
      num2     = fromIntegral $ 2^bit2
      mask     = complement num2

      nl      = 2 * nl2 
      nl2     = (len (f (Array (\j -> arr ! variable "X") n2 )))

logInt n | n <= 1 = 0
logInt n = 1 + logInt (n `div` 2) 


sh :: (Bits a) => Int -> Int -> a -> a
sh b1 b2 a | b1 == b2 = a  
           | b1 <  b2 = a `shiftR` (b2 - b1)
           | b1 >  b2 = a `shiftL` (b1 - b2)



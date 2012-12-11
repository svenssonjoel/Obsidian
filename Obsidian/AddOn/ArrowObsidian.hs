{-# LANGUAGE TypeOperators, 
             GADTs, 
             FlexibleContexts #-}

{- Joel Svensson 2012

   Notes:

   2012-12-10: Updated (now loads successfully again)
-} 

module Obsidian.AddOn.ArrowObsidian where 


import Obsidian.Exp 
--import Obsidian.Kernel
import Obsidian.Program hiding (Sync)
import Obsidian.Array
import Obsidian.Force
--import Obsidian.Sync

import Control.Category
import Prelude hiding ((.),id)

import Data.Bits
import Data.Word

{- an attempt at implementing old ArrowObsidian as a 
   library on top of new Obsidian -} 


data a :-> b where 
  Pure :: (a -> b) -> (a :-> b) 
  Sync :: Forceable (Pull (Exp b)) 
          => (a -> Pull (Exp b)) 
          -> (Pull (Exp b) :-> c) 
          -> (a :-> c) 
          

runArrow :: (a :-> b) -> a -> BProgram b 
runArrow (Pure f) a   = return$ f a
runArrow (Sync f g) a = 
  do 
    a' <- force (f a) 
    runArrow g a'          
          
                    
instance Category (:->) where
  id = Pure id 
  
  (.) (Pure f) (Pure g) = Pure (f . g) 
  (.) (Pure f) (Sync h g) = Sync h (Pure f . g)
  (.) (Sync h g) (Pure f) = Sync (h . f) g 
          

-- More or less means that only "Exps" are ever allowed..
-- Try to generalise. Force is to blame for this.. 
aSync :: Forceable (Pull (Exp a)) =>  Pull (Exp a) :-> Pull (Exp a) 
aSync = Sync id (Pure id)

----------------------------------------------------------------------------
-- Library
two :: (Pull a :-> Pull b) -> (Pull a :-> Pull b)
two (Pure f)  = Pure $ twoFF f
two (Sync f g) = Sync (twoFF f) (two g)

twoFF :: (Pull a -> Pull b) -> Pull a -> Pull b
twoFF f arr = 
    Pull nl (\i -> f (
          Pull  n2 (\j -> arr ! ((sh bit bit2 (i .&. num2)) .|. j))) ! 
                                    (i .&. mask))
    where 
      n2       = (len arr) `div` 2 :: Word32
      bit      = logInt n2 

      bit2     = logInt nl2
      num2     = fromIntegral $ 2^bit2
      mask     = complement num2

      nl      = 2 * nl2 
      nl2     = (len (f (Pull n2 (\j -> arr ! variable "X"))))

logInt n | n <= 1 = 0
logInt n = 1 + logInt (n `div` 2) 


sh :: (Bits a) => Int -> Int -> a -> a
sh b1 b2 a | b1 == b2 = a  
           | b1 <  b2 = a `shiftR` (b2 - b1)
           | b1 >  b2 = a `shiftL` (b1 - b2)



{-# LANGUAGE MultiParamTypeClasses,  
             FlexibleInstances, FlexibleContexts,
             GADTs, 
             TypeFamilies,
             RankNTypes #-} 

{- Joel Svensson 2012

   Notes:
    2013-01-08: Removed number-of-blocks field from Distribs
    2012-12-10: Drastically shortened. 
-}

module Obsidian.Array  where

import Obsidian.Exp 
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Program
import Obsidian.Names

import Data.List
import Data.Word

---------------------------------------------------------------------------
-- Aliases
---------------------------------------------------------------------------
type SPull = Pull Word32
type DPull = Pull EWord32

type SPush t a = Push t Word32 a
type DPush t a = Push t EWord32 a 
---------------------------------------------------------------------------
-- Create arrats
---------------------------------------------------------------------------
undefinedGlobal n = Pull n $ \gix -> undefined
namedGlobal name n = Pull n $ \gix -> index name gix
namedPull name n = Pull n $ \gix -> index name gix

---------------------------------------------------------------------------
-- Class ArraySize
--------------------------------------------------------------------------- 
class (Integral a, Num a) => ASize a where
  sizeConv :: a ->  Exp Word32

instance ASize Word32 where
  sizeConv = fromIntegral

instance ASize (Exp Word32) where
  sizeConv = id 

---------------------------------------------------------------------------
-- Push and Pull arrays
---------------------------------------------------------------------------
data Push p s a =
  Push s (forall b. ((a -> Exp Word32 -> TProgram NameInfo) -> Program p NameInfo))

data Pull s a = Pull {pullLen :: s, 
                      pullFun :: Exp Word32 -> a}

mkPushArray :: s -> (forall b. ((a -> Exp Word32 -> TProgram NameInfo)
                             -> Program t NameInfo)) -> Push t s a
mkPushArray n p = Push n p 
mkPullArray n p = Pull n p  

class Array a where
  resize :: r -> a s e -> a r e
  len    :: ASize s => a s e -> s
  aMap   :: (e -> e') -> a s e -> a s e'
  ixMap  :: (Exp Word32 -> Exp Word32)
            -> a s e -> a s e
  
instance Array Pull where 
  resize m (Pull _ ixf) = Pull m ixf
  len      (Pull s _)   = s
  aMap   f (Pull n ixf) = Pull n (f . ixf)
  ixMap  f (Pull n ixf) = Pull n (ixf . f) 
  
instance Array (Push t) where 
  resize m (Push _ p) = Push m p
  len      (Push s _) = s
  aMap   f (Push s p) = Push s $ \wf -> p (\e ix -> wf (f e) ix)
  ixMap  f (Push s p) = Push s $ \wf -> p (\e ix -> wf e (f ix))
  

class Indexible a where 
  access :: a s e -> Exp Word32 -> e 
  
instance Indexible Pull where
  access p ix = pullFun p ix

---------------------------------------------------------------------------
-- Functor instance Pull/Push arrays
---------------------------------------------------------------------------
instance Array arr => Functor (arr w) where 
  fmap = aMap


---------------------------------------------------------------------------
-- Pushable
---------------------------------------------------------------------------
class Pushable a where 
  push  :: ASize s => PT t -> a s e -> Push t s e
  -- Push using m threads
  --  m must be a divisor of nm  (TODO: error otherwise) 
  --pushN :: Word32 -> a e -> Push e

  -- push grouped elements to adjacent indices using
  -- one thread per group. 
  --pushF :: a [e] -> Push e 
 
instance Pushable (Push Thread) where 
  push Thread = id
  push Block  = error "not implemented: Program transformation!"
  push Grid   = error "not implemented: Program transformation!" 
instance Pushable (Push Block) where 
  push Block = id 
  push Thread = error "not implemented: Program transformations!"
  push Grid = error "not implemented: Program transformations!" 
instance Pushable (Push Grid) where 
  push Grid = id
  push Thread = error "not implemented: Program transformations!"
  push Block = error "not implemented: Program transformations!" 
  
instance Pushable Pull where
  push Thread (Pull n ixf) =
    Push n $ \wf -> seqFor (sizeConv n) $ \i -> wf (ixf i) i
  push Block (Pull n ixf) =
    Push n $ \wf -> ForAll (sizeConv n) $ \i -> wf (ixf i) i 
  push Grid (Pull n ixf) =
    Push n $ \wf -> ForAllThreads (sizeConv n) $ \i -> wf (ixf i) i 

{-  pushN m (Pull nm ixf) =
    Push nm -- There are still nm elements (info for alloc) 
    $ \wf ->
    ForAll (Just m) 
    $ \i ->
    sequence_ [wf (ixf (i + fromIntegral (j * n))) (i + (fromIntegral (j * n)))
              | j <- [0..n]] 
    -- Force can still Allocate n elements for this Push array.
    where
      n = fromIntegral (nm `div` m)

  pushF (Pull n ixf) =
    Push (n * m) $ \wf ->
    ForAll (Just n) $ \i ->
    sequence_ [wf ((ixf i) !! fromIntegral j)  (i * fromIntegral m + fromIntegral j)
              | j <- [0..m]]
    where 
      m = fromIntegral$ length $ ixf 0
-} 

{- ------------------------------------------------------------------------

     m     m     m     m     m 
  |-----|-----|-----|-----|-----| (n * m)

   01234 01234 01234 01234 01234  k 

  0     1     2     3     4       j

  m threads, each writing n elements:
  [(tid + (j*m) | j <- [0..n]] 

  n threads, each writing m elements:
  [(tid * m + k | k <- [0..m]] 

------------------------------------------------------------------------ -}   
---------------------------------------------------------------------------
-- Indexing, array creation.
---------------------------------------------------------------------------
namedArray name n = mkPullArray n (\ix -> index name ix)
indexArray n      = mkPullArray n (\ix -> ix)

pushApp (Push _ p) a = p a 

infixl 9 ! 
(!) :: Indexible a => a s e -> Exp Word32 -> e 
(!) = access

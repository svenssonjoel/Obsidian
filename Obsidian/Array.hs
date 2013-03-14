{-# LANGUAGE MultiParamTypeClasses,  
             FlexibleInstances, FlexibleContexts,
             GADTs, 
             TypeFamilies #-} 

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


import Data.List
import Data.Word

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
data Push p s a = Push s
                     ((a -> Exp Word32 -> TProgram ()) -> Program p ())   

data Pull s a = Pull {pullLen :: s, 
                      pullFun :: Exp Word32 -> a}

--type PushArray a = Push a
--type PullArray a = Pull a

mkPushArray :: s -> ((a -> Exp Word32 -> TProgram ())
                             -> Program t ()) -> Push t s a
mkPushArray n p = Push n p 
mkPullArray n p = Pull n p  

class Array a where
  resize :: s -> a s e -> a s e
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
    Push n $ \wf -> SeqFor (sizeConv n) $ \i -> wf (ixf i) i
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
-- Global Pushable
--------------------------------------------------------------------------- 
{- 
class PushableGlobal a where
  pushG :: a e -> GlobPush e
  -- Push Global and Flatten
  pushGF :: a [e] -> GlobPush e

  -- Push using m threads per block and n elements per thread
  pushGN :: Word32 -> Word32 -> a e -> GlobPush e
  
instance PushableGlobal GlobPull where
  pushG (GlobPull ixf) =
      GlobPush 
        $ \wf -> forAllT
                 $ \gix -> wf (ixf gix) gix
                           
  pushGF (GlobPull ixf) = undefined

  -- Implementing this will set a fixed blocksize.
  -- But then again, if exact control over number of threads
  -- is wanted then that is neccessary. 
  pushGN m n (GlobPull ixf) =
    GlobPush 
    $ \wf ->
    ForAllBlocks $ \bid ->
    ForAll (Just m) 
    $ \tid ->
    let i = bid * (fromIntegral m) + tid in     
    sequence_ [wf (ixf (i + fromIntegral (j * n))) (i + fromIntegral (j * n))
              | j <- [0..n]] 
    

-} 
  
---------------------------------------------------------------------------
-- Indexing, array creation.
---------------------------------------------------------------------------
namedArray name n = mkPullArray n (\ix -> index name ix)
indexArray n      = mkPullArray n (\ix -> ix)


--instance Indexible GlobPull a where
--  access (GlobPull ixf) ix = ixf ix 

--instance Indexible Distrib a where
--  access p ix = getBlock p ix 

pushApp (Push _ p) a = p a 

infixl 9 ! 
(!) :: Indexible a => a s e -> Exp Word32 -> e 
(!) = access

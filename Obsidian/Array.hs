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
-- A value that can not be used in further computations
--------------------------------------------------------------------------- 
data Final a = Final {cheat :: a} -- cheat should not be exposed. 


---------------------------------------------------------------------------
-- Global result array. 
---------------------------------------------------------------------------

data GlobPush a =
  GlobPush (( a -> Exp Word32 -> TProgram ()) -> GProgram ())

--data GlobPush2 a =
--  GlobPush2 Word32
--        ((a -> Exp Word32 -> Exp Word32 -> TProgram ()) ->
--         GProgram ())


-- Conversions between kinds of global push arrays 
--globView :: GlobPush2 a -> GlobPush a
--globView (GlobPush2 n pushf) = GlobPush n pushf'
-- where
--    pushf' wf = pushf $ \a bix tix -> wf a (bix * fromIntegral n + tix)

--blockView :: GlobPush a -> GlobPush2 a
--blockView (GlobPush n pushf) = GlobPush2 n pushf'
--  where
--    pushf' wf = pushf $ \a gix -> wf a (gix `div` fromIntegral n)
--                                       (gix `mod` fromIntegral n) 


---------------------------------------------------------------------------
-- Experiment
---------------------------------------------------------------------------
data GlobPull a = GlobPull (Exp Word32 -> a)


-- Takes a block id and gives you what that block computes. 
data DistPull a = DistPull (Exp Word32 -> BProgram a)

undist :: DistPull (Pull a) -> GlobPush a
undist (DistPull bixf) =
  GlobPush $ \wf ->
  ForAllBlocks $ \bix ->
  do
    pully <- bixf bix
    let n = len pully 
    ForAll (Just n) $ \tix ->
      wf (pully ! (bix * fromIntegral n + tix))
         (bix * fromIntegral n + tix) 
                          

-- replaces Distrib ? 
-- data GlobPull2 a = GlobPull2 Word32 (Exp Word32 -> Exp Word32 -> a)

-- Create global pull arrays 
undefinedGlobal = GlobPull $ \gix -> undefined
namedGlobal name = GlobPull $ \gix -> index name gix


--sizedGlobal2 bs = GlobPull2 bs $ \bix tix -> undefined
--namedGlobal2 name bs = GlobPull2 bs
--                       $ \gix tix -> index name (gix * fromIntegral bs + tix) 

---------------------------------------------------------------------------
-- Push and Pull arrays
---------------------------------------------------------------------------
data Push a = Push Word32
                   ((a -> Exp Word32 -> TProgram ()) -> BProgram ())   

data Pull a = Pull {pullLen :: Word32, 
                    pullFun :: Exp Word32 -> a}

--type PushArray a = Push a
--type PullArray a = Pull a

mkPushArray :: Word32 -> ((a -> Exp Word32 -> TProgram ())
                         -> BProgram ()) -> Push a
mkPushArray n p = Push n p 
mkPullArray n p = Pull n p  

class Resizeable a where
  resize :: Word32 -> a e -> a e 

instance Resizeable Pull where 
  resize m (Pull _ ixf) = Pull m ixf
  
instance Resizeable Push where 
  resize m (Push _ p) = Push m p  

---------------------------------------------------------------------------
-- Pushable
---------------------------------------------------------------------------
class Pushable a where 
  push  :: a e -> Push e
  -- Push using m threads
  --  m must be a divisor of nm  (TODO: error otherwise) 
  pushN :: Word32 -> a e -> Push e

  -- push grouped elements to adjacent indices using
  -- one thread per group. 
  pushF :: a [e] -> Push e 
 
instance Pushable Push where 
  push = id
  pushN = error "pushN on Push array: don't know how to implement that yet" 
  pushF = error "pushF on Push array: don't know hot to implement that yet" 
  
instance Pushable Pull where   
  push (Pull n ixf) =
    Push n $ \wf -> ForAll (Just n) $ \i -> wf (ixf i) i

  pushN m (Pull nm ixf) =
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
    


  
---------------------------------------------------------------------------
-- Indexing, array creation.
---------------------------------------------------------------------------
namedArray name n = mkPullArray n (\ix -> index name ix)
indexArray n      = mkPullArray n (\ix -> ix)

class Indexible a e where 
  access :: a e -> Exp Word32 -> e 
  
instance Indexible Pull a where
  access p ix = pullFun p ix

instance Indexible GlobPull a where
  access (GlobPull ixf) ix = ixf ix 

--instance Indexible Distrib a where
--  access p ix = getBlock p ix 

pushApp (Push _ p) a = p a 

class Len a where 
  len :: a e -> Word32

instance Len Pull where 
  len (Pull n _) = n

instance Len Push where
  len (Push n _) = n




infixl 9 ! 
(!) :: Indexible a e => a e -> Exp Word32 -> e 
(!) = access

{-# LANGUAGE MultiParamTypeClasses,  
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances,
             GADTs  #-} 

{- Joel Svensson 2012

   Notes:
    2013-02-14 : Complete rewrite
      Adding shapes and investigating some ideas put forward by Niklas Ulvinge.

-}

module Obsidian.Array  where

import Obsidian.Exp 
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Program

import Data.List
import Data.Word

import Obsidian.Shape

---------------------------------------------------------------------------
-- Push and Pull arrays 
--------------------------------------------------------------------------- 

data Pull pt sh a = Pull sh (E sh -> a)
data Push pt sh a = Push sh (((E sh, a) -> Program Thread ()) -> Program pt ())

-- Accessing is only cheap on pull arrays! 
class Access arr pt sh where
  access :: arr pt sh e -> E sh -> e 

instance Access Pull pt sh where
  access (Pull _ shf) ix = shf ix

-- Monadic Access functionality (very costly in push array case) 
class AccessP arr pt sh  where
  accessM :: arr pt sh  e -> E sh -> Program pt e 

instance AccessP Pull pt sh where
  accessM (Pull _ shf) ix = return $ shf ix 

instance AccessP Push pt sh where
  accessM push = error "accessM: TODO - needs the force" 

class Array arr pt sh where
  len :: arr pt sh e -> sh 
  resize :: arr pt sh e -> sh -> arr pt sh e 
  aMap :: (e -> e') -> arr pt sh e ->  arr pt sh e' 
  ixMap :: (E sh -> E sh) -> arr pt sh e ->  arr pt sh e 


instance Array Pull pt sh where 
  len    (Pull sh _) = sh
  resize (Pull _ shf) sh = Pull sh shf
  aMap   f (Pull sh shf)  = Pull sh (f . shf)
  ixMap  f (Pull sh shf)  = Pull sh (shf . f) 


instance Array Push pt sh where
  len    (Push sh _) = sh
  resize (Push _ shf) sh = Push sh shf
  aMap   f (Push sh pushf) = 
   Push sh $ \wf -> pushf (\(ix, a) -> wf (ix, f a))
  ixMap  f (Push sh pushf) = 
   Push sh $ \wf -> pushf (\(ix, a) -> wf (f ix, a)) 


---------------------------------------------------------------------------
-- Lets see how pushable works out in this setting. 
--------------------------------------------------------------------------- 

class Pushable a pt sh where
  push :: a pt sh e -> Push pt sh e  

instance Shapely sh => Pushable Pull Block sh where
   push (Pull sh ixf) = 
     let n = size sh 
     in  Push sh $ \wf -> ForAll n $ \i -> wf (fromIndex sh i,ixf (fromIndex sh i))

instance Shapely sh => Pushable Pull Thread sh where
   push (Pull sh ixf) = 
     let n = size sh 
     in  Push sh $ \wf -> SeqFor n $ \i -> wf (fromIndex sh i,ixf (fromIndex sh i))

instance Shapely sh => Pushable Pull Grid sh where
   push (Pull sh ixf) = 
     let n = size sh
         -- Im not sure about the number of threads to use here.
     in  Push sh $ \wf -> ForAllGlobal n
                          $ \i -> wf (fromIndex sh i,ixf (fromIndex sh i))


---------------------------------------------------------------------------
-- Running local computations over global arrays 
---------------------------------------------------------------------------
--
-- The interesting interactions between Grid, Block and Thread arrays 
-- are when a grid is split up into blocks, and blocks divided over threads.
-- These things are not captured by the "push" functions above.


blocks :: (Static sh1, Blockable sh1 sh2 sh3)
          => (Pull Block sh1 a -> BProgram b)
          -> sh1
          -> Pull Grid sh2 a  -- these shapes needs to be "compatible" 
          -> (sh3 -> BProgram b)
blocks f sh1 (Pull sh2 shf) bix =
  -- would now want to "SLICE" out sh1 shaped things
  -- from the array shaped as sh2.
  let pullb = Pull sh1 (\ix -> shf (block sh1 sh2 bix ix))
  in  f pullb



---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------
namedGlobal sh name = Pull sh $ \gix -> index name (toIndex sh gix)


{- 
---------------------------------------------------------------------------
-- A value that can not be used in further computations
--------------------------------------------------------------------------- 
data Final a = Final {cheat :: a} -- cheat should not be exposed.

---------------------------------------------------------------------------
-- Global result array. 
---------------------------------------------------------------------------

data GlobPush a =
  GlobPush (( a -> Exp Word32 -> TProgram ()) -> GProgram ())

---------------------------------------------------------------------------
-- Experiment
---------------------------------------------------------------------------
data GlobPull a = GlobPull (Exp Word32 -> a)


-- Takes a block id and gives you what that block computes. 
data DistPull a = DistPull (Exp Word32 -> BProgram a)

 
-- Desired type (not sure). 
--undist :: DistPull (Pull a) -> GProgram (GlobPush a)
undist :: DistPull (Pull a) -> GlobPush a
undist (DistPull bixf) =
  GlobPush $ \wf ->
  ForAllBlocks $ \bix ->
  do
    pully <- bixf bix
    let n = len pully 
    ForAll (Just n) $ \tix ->
      wf (pully ! tix)
         (bix * fromIntegral n + tix) 
                          

-- Create global pull arrays 
undefinedGlobal = GlobPull $ \gix -> undefined
namedGlobal name = GlobPull $ \gix -> index name gix


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

--class Resizeable a where
--  resize :: Word32 -> a e -> a e 

--instance Resizeable Pull where 
--  resize m (Pull _ ixf) = Pull m ixf
  
--instance Resizeable Push where 
--  resize m (Push _ p) = Push m p  

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

--class Indexible a e where 
--  access :: a e -> Exp Word32 -> e 
  
--instance Indexible Pull a where
--  access p ix = pullFun p ix

--instance Indexible GlobPull a where
--  access (GlobPull ixf) ix = ixf ix 

--instance Indexible Distrib a where
--  access p ix = getBlock p ix 

pushApp (Push _ p) a = p a 

--class Len a where 
--  len :: a e -> Word32

--instance Len Pull where 
--  len (Pull n _) = n

--instance Len Push where
--  len (Push n _) = n




infixl 9 ! 
(!) :: Array arr pt sh  => arr pt sh e -> sh -> e 
(!) = access
-}

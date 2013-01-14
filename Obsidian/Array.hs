{-# LANGUAGE MultiParamTypeClasses,  
             FlexibleInstances, FlexibleContexts,
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
-- An Array distributed over MultiProcessors (same as old Blocks) 
---------------------------------------------------------------------------
data Distrib a = Distrib (Exp Word32 -> a)

getBlock :: Distrib a -> Exp Word32 -> a 
getBlock (Distrib bixf) = bixf

sizedGlobal bs = Distrib (\bix -> (mkPullArray bs undefined))
namedGlobal name bs = Distrib  
                      (\bix -> (mkPullArray bs
                                (\ix -> index name (bix * (fromIntegral bs) + ix)))) 


type DistArray a = Distrib (Pull a) 

---------------------------------------------------------------------------
-- Global result array. 
---------------------------------------------------------------------------
data GlobPush a =
  GlobPush Word32
        ((a -> Exp Word32 -> Exp Word32 -> TProgram ()) ->
         GProgram ())

data GlobPush' a =
  GlobPush' Word32
            (( a -> Exp Word32 -> TProgram ()) -> GProgram ())


conv1 :: GlobPush a -> GlobPush' a
conv1 (GlobPush n pushf) = GlobPush' n pushf'
  where
    pushf' wf = pushf $ \a bix tix -> wf a (bix * fromIntegral n + tix)

conv2 :: GlobPush' a -> GlobPush a
conv2 (GlobPush' n pushf) = GlobPush n pushf'
  where
    pushf' wf = pushf $ \a gix -> wf a (gix `div` fromIntegral n)
                                       (gix `mod` fromIntegral n) 


---------------------------------------------------------------------------
-- Experiment
---------------------------------------------------------------------------
data GlobPull a = GlobPull Word32 (Exp Word32 -> a)

-- replaces Distrib ? 
data GlobPull2 a = GlobPull2 Word32 (Exp Word32 -> Exp Word32 -> a) 
---------------------------------------------------------------------------
-- Push and Pull arrays
---------------------------------------------------------------------------
data Push a = Push Word32
                   ((a -> Exp Word32 -> TProgram ()) -> BProgram ())   

data Pull a = Pull {pullLen :: Word32, 
                    pullFun :: Exp Word32 -> a}

type PushArray a = Push a
type PullArray a = Pull a

mkPushArray :: Word32 -> ((a -> Exp Word32 -> TProgram ())
                         -> BProgram ()) -> PushArray a
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
  pushS :: Word32 -> a e -> Push e 

instance Pushable Push where 
  push = id
  pushS = error "pushS on Push array: don't know how to implement that yet" 
  
instance Pushable Pull where   
  push (Pull n ixf) =
    Push n $ \wf -> ForAll n $ \i -> wf (ixf i) i
  pushS m (Pull n ixf) =
    -- Force can still Allocate n elements for this Push array.
    Push n $ \wf -> ForAll nP
                    $ \i -> SeqFor nS $ \j -> wf (ixf (i*nS + j)) (i*nS + j)
    where
      nS = fromIntegral m
      nP = fromIntegral (n `div` m)

---------------------------------------------------------------------------
-- Global Pushable
--------------------------------------------------------------------------- 

class PushableGlobal a where
  pushG :: a e -> GlobPush e 
  pushGF :: a [e] -> GlobPush e

instance PushableGlobal GlobPull where
  pushG (GlobPull n ixf) =
      GlobPush n
        $ \wf -> ForAllBlocks 
                 $ \ bix -> ForAll n
                            $ \ ix -> wf (ixf (bix * fromIntegral n + ix)) bix ix
  pushGF (GlobPull n ixf) = undefined

instance PushableGlobal GlobPull2 where
  pushG (GlobPull2 n ixf) =
      GlobPush n
        $ \wf -> ForAllBlocks 
                 $ \ bix -> ForAll n
                            $ \ ix -> wf (ixf bix ix) bix ix
  pushGF (GlobPull2 n bixixf) = undefined 

  
---------------------------------------------------------------------------
-- Indexing, array creation.
---------------------------------------------------------------------------
namedArray name n = mkPullArray n (\ix -> index name ix)
indexArray n      = mkPullArray n (\ix -> ix)

class Indexible a e where 
  access :: a e -> Exp Word32 -> e 
  
instance Indexible Pull a where
  access p ix = pullFun p ix

instance Indexible Distrib a where
  access p ix = getBlock p ix 

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

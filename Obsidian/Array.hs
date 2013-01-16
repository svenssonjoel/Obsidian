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
  pushS :: Word32 -> a e -> Push e 

instance Pushable Push where 
  push = id
  pushS = error "pushS on Push array: don't know how to implement that yet" 
  
instance Pushable Pull where   
  push (Pull n ixf) =
    Push n $ \wf -> ForAll (Just n) $ \i -> wf (ixf i) i
  pushS m (Pull n ixf) =
    -- Force can still Allocate n elements for this Push array.
    Push n $ \wf -> ForAll (Just nP)
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
  pushG (GlobPull ixf) =
      GlobPush 
        $ \wf -> forAllT
                 $ \gix -> wf (ixf gix) gix 
  pushGF (GlobPull ixf) = undefined

  
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

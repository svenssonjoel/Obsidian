{-# LANGUAGE MultiParamTypeClasses,  
             FlexibleInstances, FlexibleContexts,
             TypeFamilies #-} 

{- Joel Svensson 2012

   Notes:
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
data Distrib a = Distrib (Exp Word32)
                         (Exp Word32 -> a)

numBlocks :: Distrib a -> Exp Word32
numBlocks (Distrib n _) = n

getBlock :: Distrib a -> Exp Word32 -> a 
getBlock (Distrib _ bixf) = bixf

sizedGlobal bn bs = Distrib bn
                    (\bix -> (mkPullArray bs undefined))
namedGlobal name bn bs = Distrib bn 
                         (\bix -> (mkPullArray bs
                                   (\ix -> index name (bix * (fromIntegral bs) + ix)))) 


type DistArray a = Distrib (Pull a) 

---------------------------------------------------------------------------
-- Sequential arrays
--
-- Ok. Sequential arrays are problematic.
--   Especially if the can have dynamic length.
--   Sequential computation must be handled differently.
--   Knowing how much memory to allocate is, for an array, otherwise
--   problematic.
--   
---------------------------------------------------------------------------
--data Seq a = Seq (Exp Word32) (Exp Word32 -> a)

--data SPush a = SPush (Exp Word32)
--                     ((a -> Exp Word32 -> TProgram ()) -> TProgram ())

--sPush :: Seq a -> SPush a
--sPush (Seq n ixf) =  
--  SPush n $ \wf -> SeqFor n $ \i -> wf (ixf i) i
---------------------------------------------------------------------------
-- Global result array. 
---------------------------------------------------------------------------
data GPush a =
  GPush (Exp Word32)
        Word32
        ((a -> Exp Word32 -> Exp Word32 -> TProgram ()) ->
         GProgram ())

type GlobArray a = GPush a 
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

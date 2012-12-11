{-# LANGUAGE MultiParamTypeClasses,  
             FlexibleInstances  #-} 

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

sizedGlobal bn bs = Distrib bn
                    (\bix -> (mkPullArray bs undefined))
namedGlobal name bn bs = Distrib bn 
                         (\bix -> (mkPullArray bs
                                   (\ix -> index name (bix * (fromIntegral bs) + ix)))) 

---------------------------------------------------------------------------
-- Global result array. 
---------------------------------------------------------------------------
data GlobArray a =
  GlobArray (Exp Word32)
            Word32
            ((a -> Exp Word32 -> Exp Word32 -> TProgram ()) ->
             GProgram ())
                             
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
  push :: a e -> Push e 

instance Pushable Push where 
  push = id 
  
instance Pushable Pull  where   
  push (Pull n ixf) =
    Push n $ \wf -> BForAll n $ \i -> wf (ixf i) i

---------------------------------------------------------------------------
-- Indexing, array creation.
---------------------------------------------------------------------------
namedArray name n = mkPullArray n (\ix -> index name ix)
indexArray n      = mkPullArray n (\ix -> ix)

class Indexible a e where 
  access :: a e -> Exp Word32 -> e 
  
instance Indexible Pull a where
  access p ix = pullFun p ix


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

{-# LANGUAGE MultiParamTypeClasses,  
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances,
             GADTs,
             ScopedTypeVariables #-} 

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

data Pull sh a = Pull sh (IxTy sh -> a)
data Push pt sh a = Push sh (((IxTy sh, a) -> Program Thread ()) -> Program pt ())

-- Accessing is only cheap on pull arrays! 
class Access arr sh where
  access :: arr sh e -> IxTy sh -> e 

instance Access Pull sh where
  access (Pull _ shf) ix = shf ix

-- Monadic Access functionality (very costly in push array case) 
class AccessP arr sh where
  accessM :: arr sh e -> IxTy sh -> Program pt e 

instance AccessP Pull sh where
  accessM (Pull _ shf) ix = return $ shf ix 

instance AccessP (Push pt) sh where
  accessM push = error "accessM: TODO - needs the force" 

class Array arr sh where
  shape  :: arr sh e -> sh 
  resize :: arr sh e -> sh -> arr sh e 
  aMap   :: (e -> e') -> arr sh e ->  arr sh e' 
  ixMap  :: (IxTy sh -> IxTy sh) -> arr sh e ->  arr sh e 


instance Array Pull sh where 
  shape  (Pull sh _) = sh
  resize (Pull _ shf) sh = Pull sh shf
  aMap   f (Pull sh shf)  = Pull sh (f . shf)
  ixMap  f (Pull sh shf)  = Pull sh (shf . f) 


instance Array (Push pt) sh where
  shape  (Push sh _) = sh
  resize (Push _ shf) sh = Push sh shf
  aMap   f (Push sh pushf) = 
   Push sh $ \wf -> pushf (\(ix, a) -> wf (ix, f a))
  ixMap  f (Push sh pushf) = 
   Push sh $ \wf -> pushf (\(ix, a) -> wf (f ix, a)) 


---------------------------------------------------------------------------
-- Lets see how pushable works out in this setting. 
--------------------------------------------------------------------------- 

--push (Pull sh ixf) =
--  let n = fromIntegral $ size sh
--  in  Push sh $ \wf -> ForAll n $ \i -> wf (fromIndex sh i,ixf (fromIndex sh i))


class Pushable p where
  push :: (IxTy i ~ Exp Word32, Shape sh i)
          => PT t -> p (sh i) e -> Push t (sh i) e  

instance Pushable Pull  where
   push Grid (Pull sh ixf) = 
     let n = size sh 
     in  Push sh $ \wf -> ForAllGlobal n $ \i -> wf (fromIndex sh i,ixf (fromIndex sh i))
   push Block (Pull sh ixf) = 
     let n = size sh 
     in  Push sh $ \wf -> ForAll n $ \i -> wf (fromIndex sh i,ixf (fromIndex sh i))
   push Thread (Pull sh ixf) =
     let n = size sh 
     in  Push sh $ \wf -> SeqFor n $ \i -> wf (fromIndex sh i,ixf (fromIndex sh i))
                                           
   
instance Pushable (Push Block) where
   push Block p = p

instance Pushable (Push Thread) where
   push Thread p = p

instance Pushable (Push Grid) where
   push Grid p = p





--instance Shape sh Word32 => Pushable Pull Thread (sh Word32) where
--   push (Pull sh ixf) = 
--     let n = fromIntegral $ size sh 
--     in  Push sh $ \wf -> SeqFor n $ \i -> wf (fromIndex sh i,ixf (fromIndex sh i))

--instance Shape sh Word32 => Pushable Pull Grid (sh Word32) where
--   push (Pull sh ixf) = 
--     let n = fromIntegral $ size sh
--         -- Im not sure about the number of threads to use here.
--     in  Push sh $ \wf -> ForAllGlobal n
--                          $ \i -> wf (fromIndex sh i,ixf (fromIndex sh i))



---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------
namedGlobal sh name = Pull sh $ \gix -> index name (toIndex sh gix)

(!) :: Access arr sh => arr sh a -> IxTy sh -> a 
(!) = access 




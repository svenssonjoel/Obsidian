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

module Obsidian.Array (Pull, Push, SPull, DPull, SPush, DPush,
                       Pushable, 
                       mkPull,
                       mkPush,
                       push,
                       pushN,
                       setSize, 
                       (!),
                       (<:), 
                       Array(..),
                       ASize(..),
                       namedGlobal,
                       undefinedGlobal) where

import Obsidian.Exp 
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Program

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
-- Create arrays
---------------------------------------------------------------------------
-- | An undefined array. Use as placeholder when generating code
undefinedGlobal n = Pull n $ \gix -> undefined
-- | A named global array. 
namedGlobal name n = Pull n $ \gix -> index name gix
-- namedPull name n = Pull n $ \gix -> index name gix

---------------------------------------------------------------------------
-- Class ArraySize
---------------------------------------------------------------------------
-- | ASize provides conversion to Exp Word32 for array sizes
class (Integral a, Num a) => ASize a where
  sizeConv :: a ->  Exp Word32

instance ASize Word32 where
  sizeConv = fromIntegral

instance ASize (Exp Word32) where
  sizeConv = id 

---------------------------------------------------------------------------
-- Push and Pull arrays
---------------------------------------------------------------------------
-- | Push array. Parameterised over Program type and size type.
data Push p s a =
  Push s ((a -> EWord32 -> TProgram ()) -> Program p ())

-- -- Is this useful for anything ? 
-- data PPush t s a =
--   PPush s ((a -> EWord32 ->  Program (Below t) ()) -> Program t ())

-- | Pull array.
data Pull s a = Pull {pullLen :: s, 
                      pullFun :: EWord32 -> a}

-- | Create a push array. 
mkPush :: s -> ((a -> EWord32 -> TProgram ())
                             -> Program t ()) -> Push t s a
mkPush n p = Push n p 

-- | Create a pull array. 
mkPull n p = Pull n p 

setSize :: l -> Pull l a -> Pull l a
setSize n (Pull _ ixf) = mkPull n ixf

-- Fix this.
--   * you cannot safely resize either push or pull arrays
--   * you can shorten pull arrays safely. 
class Array a where
  len    :: a s e -> s
  aMap   :: (e -> e') -> a s e -> a s e'
  ixMap  :: (Exp Word32 -> Exp Word32)
            -> a s e -> a s e
  
instance Array Pull where 
  len      (Pull s _)   = s
  aMap   f (Pull n ixf) = Pull n (f . ixf)
  ixMap  f (Pull n ixf) = Pull n (ixf . f) 
  
instance Array (Push t) where 
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

class Pushable t where
  push :: ASize s => Pull s e -> Push t s e 

instance Pushable Thread where
  push (Pull n ixf) =
    Push n $ \wf -> seqFor (sizeConv n) $ \i -> wf (ixf i) i

instance Pushable Block where
  push (Pull n ixf) =
    Push n $ \wf -> ForAll (sizeConv n) $ \i -> wf (ixf i) i


class PushableN t where
  pushN :: ASize s => Word32 -> Pull s e -> Push t s e

instance PushableN Block where
  pushN n (Pull m ixf) =
    Push m $ \ wf -> forAll (sizeConv (m `div` fromIntegral n)) $ \tix ->
    seqFor (fromIntegral n) $ \ix -> wf (ixf (tix * fromIntegral n + ix))
                                             (tix * fromIntegral n + ix) 
 
instance PushableN Grid where
  pushN n (Pull m ixf) =
    Push m $ \ wf -> forAll (sizeConv (m `div` fromIntegral n)) $ \bix ->
    forAll (fromIntegral n) $ \tix -> wf (ixf (bix * fromIntegral n + tix))
                                              (bix * fromIntegral n + tix) 
 
    
-- pushGrid :: Word32 ->  DPull a -> DPush Grid a
-- pushGrid m (Pull n ixf) =
--   Push n $ \ wf -> ForAll (n `div` fromIntegral m) $ \bix ->
--    ForAll (fromIntegral m) $ \tix -> wf (ixf (bix * fromIntegral m + tix))
--                                              (bix * fromIntegral m + tix)
                                     
      

---------------------------------------------------------------------------
-- Indexing, array creation.
---------------------------------------------------------------------------
--namedArray name n = mkPull n (\ix -> index name ix)
--indexArray n      = mkPull n (\ix -> ix)

pushApp (Push _ p) a = p a
infixl 9 <:
(<:) = pushApp 

infixl 9 ! 
(!) :: Indexible a => a s e -> Exp Word32 -> e 
(!) = access

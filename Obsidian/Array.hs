{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             GADTs  #-}
{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE FlexibleContexts #-}

{- Joel Svensson 2012

   Notes:
    2014-04-08: Experimenting with API 
    ---- OUTDATED ----
    2013-08-26: Experimenting with warp programs.
                These do not fit that well in established Idioms!
                TODO: Improve this situation. 
    ---- OUTDATED ----
    2013-01-08: Removed number-of-blocks field from Distribs
    2012-12-10: Drastically shortened. 
-}

module Obsidian.Array (Pull, Push, SPull, DPull, SPush, DPush
--                        Pushable, 
                      , mkPull
                      , mkPush
                      , push
                      , setSize
                      , (!)
                      , (<:)
                      , Array(..)
                      , ArrayLength(..)
                      , ASize(..)
                      , namedGlobal
                      , undefinedGlobal
                      , AsThread(..)
                      , AsWarp(..)
                      , AsBlock(..)
                      , AsGrid(..)
                      ) where

import Obsidian.Exp 
import Obsidian.Program
import Obsidian.Globs

import Prelude hiding (replicate) 
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
undefinedGlobal :: ASize s => s -> Pull s a
undefinedGlobal n = Pull n $ \_ -> undefined
-- | A named global array.
namedGlobal :: (ASize s, Scalar a) => Name -> s -> Pull s (Exp a) 
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

-- | Pull array.
data Pull s a = Pull {pullLen :: s, 
                      pullFun :: EWord32 -> a}

-- | Create a push array. 
mkPush :: s
       -> ((a -> EWord32 -> TProgram ()) -> Program t ())
       -> Push t s a
mkPush n p = Push n p 

-- | Create a pull array.
mkPull :: ASize s => s -> (EWord32 -> a) -> Pull s a
mkPull n p = Pull n p 

-- Fix this.
--   * you cannot safely resize either push or pull arrays
--   * you can shorten pull arrays safely.  
setSize :: ASize l => l -> Pull l a -> Pull l a
setSize n (Pull _ ixf) = mkPull n ixf

---------------------------------------------------------------------------
-- Array Class 
---------------------------------------------------------------------------x
class ArrayLength a where
  -- | Get the length of an array.
  len :: a s e -> s

instance ArrayLength Pull where
  len    arr  = pullLen arr

instance ArrayLength (Push t) where
  len  (Push s _) = s

class Array a where
  -- | Array of consecutive integers
  iota      :: ASize s => s -> a s EWord32
  -- | Create an array by replicating an element. 
  replicate :: ASize s => s -> e -> a s e 

  -- | Map a function over an array. 
  aMap      :: (e -> e') -> a s e -> a s e'
  -- | Perform arbitrary permutations (dangerous). 
  ixMap     :: (EWord32 -> EWord32)
               -> a s e -> a s e
  -- requires Choice !
  -- Simply because the pull array implementation of it does. 
  -- | Append two arrays. 
  append    :: (ASize s, Choice e) => a s e -> a s e -> a s e 
  
  -- technicalities
  -- | Statically sized array to dynamically sized array.
  toDyn     :: a Word32 e -> a EW32 e
  -- | Dynamically sized array to statically sized array. 
  fromDyn   :: Word32 -> a EW32 e -> a Word32 e 
  
instance Array Pull where
  iota   s = Pull s $ \ix -> ix 
  replicate s e = Pull s $ \_ -> e

  aMap   f (Pull n ixf) = Pull n (f . ixf)
  ixMap  f (Pull n ixf) = Pull n (ixf . f) 

  append a1 a2 = Pull (n1+n2)
               $ \ix -> ifThenElse (ix <* (sizeConv n1)) 
                       (a1 ! ix) 
                       (a2 ! (ix - (sizeConv n1)))
    where 
      n1 = len a1
      n2 = len a2 

  -- technicalities
  toDyn (Pull n ixf) = Pull (fromIntegral n) ixf
  fromDyn n (Pull _ ixf) = Pull n ixf 
   
  
instance Array (Push Thread) where
  iota s = Push s $ \wf ->
    do
      forAll (sizeConv s) $ \ix -> wf ix ix 
  replicate s e = Push s $ \wf ->
    do
      forAll (sizeConv s) $ \ix -> wf e ix 
  aMap   f (Push s p) = Push s $ \wf -> p (\e ix -> wf (f e) ix)
  ixMap  f (Push s p) = Push s $ \wf -> p (\e ix -> wf e (f ix))

  -- unfortunately a Choice constraint. 
  append p1 p2  =
    Push (n1 + n2) $ \wf ->
      do p1 <: wf
         p2 <: \a i -> wf a (sizeConv n1 + i) 
           where 
             n1 = len p1
             n2 = len p2 

   -- technicalities
  toDyn (Push n p) = Push (fromIntegral n) p 
  fromDyn n (Push _ p) = Push n p 

instance Array (Push Warp) where
  iota s = Push s $ \wf ->
    do
      forAll (sizeConv s) $ \ix -> wf ix ix 
  replicate s e = Push s $ \wf ->
    do
      forAll (sizeConv s) $ \ix -> wf e ix 
  aMap   f (Push s p) = Push s $ \wf -> p (\e ix -> wf (f e) ix)
  ixMap  f (Push s p) = Push s $ \wf -> p (\e ix -> wf e (f ix))

  -- unfortunately a Choice constraint. 
  append p1 p2  =
    Push (n1 + n2) $ \wf ->
      do p1 <: wf
         p2 <: \a i -> wf a (sizeConv n1 + i) 
           where 
             n1 = len p1
             n2 = len p2 

   -- technicalities
  toDyn (Push n p) = Push (fromIntegral n) p 
  fromDyn n (Push _ p) = Push n p 


instance Array (Push Block) where
  iota s = Push s $ \wf ->
    do
      forAll (sizeConv s) $ \ix -> wf ix ix 
  replicate s e = Push s $ \wf ->
    do
      forAll (sizeConv s) $ \ix -> wf e ix 
  aMap   f (Push s p) = Push s $ \wf -> p (\e ix -> wf (f e) ix)
  ixMap  f (Push s p) = Push s $ \wf -> p (\e ix -> wf e (f ix))

  -- unfortunately a Choice constraint. 
  append p1 p2  =
    Push (n1 + n2) $ \wf ->
      do p1 <: wf
         p2 <: \a i -> wf a (sizeConv n1 + i) 
           where 
             n1 = len p1
             n2 = len p2 

   -- technicalities
  toDyn (Push n p) = Push (fromIntegral n) p 
  fromDyn n (Push _ p) = Push n p 

instance Array (Push Grid) where
  iota s = error "iota: not supported as Grid" 
  replicate s e = error "replicate: not supported as Grid" 
  aMap   f (Push s p) = Push s $ \wf -> p (\e ix -> wf (f e) ix)
  ixMap  f (Push s p) = Push s $ \wf -> p (\e ix -> wf e (f ix))

  -- unfortunately a Choice constraint. 
  append p1 p2  =
    Push (n1 + n2) $ \wf ->
      do p1 <: wf
         p2 <: \a i -> wf a (sizeConv n1 + i) 
           where 
             n1 = len p1
             n2 = len p2 

   -- technicalities
  toDyn (Push n p) = Push (fromIntegral n) p 
  fromDyn n (Push _ p) = Push n p 




---------------------------------------------------------------------------
-- Functor instance Pull/Push arrays
---------------------------------------------------------------------------
instance  Array (Push t) => Functor (Push t s) where 
  fmap = aMap

instance Functor (Pull s) where
  fmap = aMap 


---------------------------------------------------------------------------
-- Pushable
---------------------------------------------------------------------------

push ::  (t *<=* Block) => ASize s => Pull s e -> Push t s e 
push (Pull n ixf) =
  Push n $ \wf ->
    forAll (sizeConv n) $ \i -> wf (ixf i) i
                                
class AsGrid a where
  asGrid :: ASize s => a s e -> Push Grid s e

instance AsGrid Pull where
  asGrid = error $ "asGrid: Trying to compute a pull array as a grid.\n" ++ 
                   "This operation is not supported, there are too many choises\n" ++
                   "involved that we just should not make automatically"
instance AsGrid (Push Grid) where
  asGrid = id

instance AsGrid (Push Block) where
  asGrid _ = error $ "asGrid: Trying to convert a Block to a Grid.\n" ++
                     "This operation is not supported!"
               
instance AsGrid (Push Warp) where 
  asGrid _ = error $ "asGrid: Trying to convert a Warp to a Grid.\n" ++
                     "This operation is not supported!"

instance AsGrid (Push Thread) where 
  asGrid _ = error $ "asGrid: Trying to convert a Thread to a Grid.\n" ++
                     "This operation is not supported!"


class AsBlock a where
  asBlock :: a Word32 e -> SPush Block e

instance AsBlock Pull where
  asBlock = push

instance AsBlock (Push Block) where
  asBlock = id

instance AsBlock (Push Grid) where
  asBlock _ = error $ "asBlock: Trying to convert a Grid to a Block.\n" ++
                      "This operation is not supported!"

instance AsBlock (Push Warp) where 
  asBlock _ = error $ "asBlock: Trying to convert a Thread to a Block.\n" ++
                      "This operation is not supported!"

instance AsBlock (Push Thread) where
  asBlock _ = error $ "asBlock: Trying to convert a Thread to a Block.\n" ++
                      "This operation is not supported!"


class AsWarp a where
  asWarp :: a Word32 e -> SPush Warp e

instance AsWarp Pull where
  asWarp = push

instance AsWarp (Push Warp) where
  asWarp = id

instance AsWarp (Push Grid) where
  asWarp _ = error $ "asWarp: Trying to convert a Grid to a Warp.\n" ++
                      "This operation is not supported!"

instance AsWarp (Push Block) where 
  asWarp _ = error $ "asWarp: Trying to convert a Block to a Warp.\n" ++
                      "This operation is not supported!"

instance AsWarp (Push Thread) where
  asWarp _ = error $ "asBlock: Trying to convert a Thread to a Warp.\n" ++
                      "This operation is not supported!"


class AsThread a where
  asThread :: a Word32 e -> SPush Thread e

instance AsThread Pull where
  asThread = push

instance AsThread (Push Thread) where
  asThread = id

instance AsThread (Push Grid) where
  asThread _ = error $ "asThread: Trying to convert a Grid to a Thread.\n" ++
                      "This operation is not supported!"

instance AsThread (Push Block) where 
  asThread _ = error $ "asThread: Trying to convert a Block to a Thread.\n" ++
                      "This operation is not supported!"

instance AsThread (Push Warp) where
  asThread _ = error $ "asThread: Trying to convert a Warp to a Thread.\n" ++
                      "This operation is not supported!"





-- class Pushable t where
--   push :: ASize s => Pull s e -> Push t s e 

-- instance Pushable Thread where
--   push (Pull n ixf) =
--     Push n $ \wf -> seqFor (sizeConv n) $ \i -> wf (ixf i) i

-- instance Pushable Warp where
--   push (Pull n ixf) =
--     Push n $ \wf ->
--       forAll (sizeConv n) $ \i -> wf (ixf i) i

-- instance Pushable Block where
--   push (Pull n ixf) =
--     Push n $ \wf ->
--       forAll (sizeConv n) $ \i -> wf (ixf i) i

-- instance Pushable Grid where
--   push (Pull n ixf) =
--     Push n $ \wf ->
--       forAll (sizeConv n) $ \i -> wf (ixf i) i 
  
-- class PushableN t where
--   pushN :: ASize s => Word32 -> Pull s e -> Push t s e

-- instance PushableN Block where
--   pushN n (Pull m ixf) =
--     Push m $ \ wf -> forAll (sizeConv (m `div` fromIntegral n)) $ \tix ->
--     warpForAll 1 $ \_ -> 
--     seqFor (fromIntegral n) $ \ix -> wf (ixf (tix * fromIntegral n + ix))
--                                              (tix * fromIntegral n + ix) 
 
-- instance PushableN Grid where
--   pushN n (Pull m ixf) =
--     Push m $ \ wf -> forAll (sizeConv (m `div` fromIntegral n)) $ \bix ->
--     forAll (fromIntegral n) $ \tix -> wf (ixf (bix * fromIntegral n + tix))
--                                               (bix * fromIntegral n + tix) 

--------------------------------------------------------------------------
-- Indexing, array creation.
---------------------------------------------------------------------------
pushApp :: Push t s a -> (a -> EWord32 -> TProgram ()) -> Program t () 
pushApp (Push _ p) a = p a

infixl 9 <:
(<:) :: Push t s a
        -> (a -> EWord32 -> Program Thread ())
        -> Program t ()
(<:) = pushApp 

infixl 9 ! 
(!) :: Pull s e -> Exp Word32 -> e 
(!) arr = pullFun arr 



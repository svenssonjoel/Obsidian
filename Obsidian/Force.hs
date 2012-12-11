
{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             ScopedTypeVariables,
             TypeFamilies #-} 

{- Joel Svensson 2012

   Notes:

   2012-12-10: Edited 

-}

module Obsidian.Force where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Array
import Obsidian.Types

---------------------------------------------------------------------------
-- New Approach to Forceable. 
---------------------------------------------------------------------------
class Forceable a where
  type Forced a 
  write_ :: a -> BProgram (Forced a)
  force  :: a -> BProgram (Forced a)  
  
---------------------------------------------------------------------------
-- Force local
---------------------------------------------------------------------------
instance Scalar a => Forceable (Pull (Exp a)) where
  type Forced (Pull (Exp a)) = Pull (Exp a)
  write_ arr = write_ (push arr) 
  force arr = force (push arr)
  

instance Scalar a => Forceable (Push (Exp a)) where
  type Forced (Push (Exp a)) = Pull (Exp a)
  write_ (Push n p) =
    do
      -- Allocate is a bit strange since
      -- it wants the n in bytes! But also knows the type. 
      name <- BAllocate (n * fromIntegral (sizeOf (undefined :: Exp a)))
                        (Pointer (typeOf (undefined :: (Exp a))))
      p (targetArr name)
      -- BSync
      return $ Pull n (\i -> index name i)
    where
      targetArr name e i = TAssign name i e

  force p =
    do
      rval <- write_ p
      BSync
      return rval
      
instance (Forceable a, Forceable b) => Forceable (a,b) where
  type Forced (a,b) = (Forced a, Forced b)
  write_ (a,b) =
    do
      r1 <- force a
      r2 <- force b 
      return (r1,r2)
  force p =
    do
      rval <- force p
      BSync
      return rval

  {- 
instance (Forceable a, Forceable b, Forceable c) => Forceable (a,b,c) where
  type Forced (a,b,c) = (Forced a, Forced b, Forced c)
  force (a,b,c) = (force a, force b, force c) 
-} 
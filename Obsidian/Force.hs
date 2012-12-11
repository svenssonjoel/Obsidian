
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
  force :: a -> Forced a


---------------------------------------------------------------------------
-- Force local
---------------------------------------------------------------------------
instance Scalar a => Forceable (Pull (Exp a)) where
  type Forced (Pull (Exp a)) = BProgram (Pull (Exp a))
  force arr = force (push arr) 

instance Scalar a => Forceable (Push (Exp a)) where
  type Forced (Push (Exp a)) = BProgram (Pull (Exp a)) 
  force (Push n p) =
    do
      -- Allocate is a bit strange since
      -- it wants the n in bytes! But also knows the type. 
    name <- BAllocate (n * fromIntegral (sizeOf (undefined :: Exp a)))
                     (Pointer (typeOf (undefined :: (Exp a))))
    p (targetArr name)
    BSync
    return $ Pull n (\i -> index name i)
    where
      targetArr name e i = TAssign name i e

instance (Forceable a, Forceable b) => Forceable (a,b) where
  type Forced (a,b) = (Forced a, Forced b)
  force (a,b) = (force a, force b) 


instance (Forceable a, Forceable b, Forceable c) => Forceable (a,b,c) where
  type Forced (a,b,c) = (Forced a, Forced b, Forced c)
  force (a,b,c) = (force a, force b, force c) 

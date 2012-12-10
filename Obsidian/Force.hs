
{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             ScopedTypeVariables,
             TypeFamilies #-} 

{- Joel Svensson 2012 -}

module Obsidian.Force where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Array
import Obsidian.Types

{- 
---------------------------------------------------------------------------
-- Class of Forceables
--------------------------------------------------------------------------- 

class Forceable a p e where
  force :: a p e -> Program (a Pull e) 


---------------------------------------------------------------------------
-- Base cases
---------------------------------------------------------------------------
instance Scalar a => Forceable Array Pull (Exp a) where
  force arr =
    do 
    name <- Allocate n $ Pointer (typeOf (undefined :: (Exp a)))
    p (targetArr name)
    Sync
    return $ Array n $ Pull (\i -> index name i)
    where
      (Array n (Push p)) = push arr 
      targetArr name (i,e) = Assign name i e

instance Scalar a => Forceable Array Push (Exp a) where
  force (Array n (Push p)) =
    do 
    name <- Allocate n $ Pointer (typeOf (undefined :: (Exp a)))
    p (targetArr name)
    Sync
    return $ Array n $ Pull (\i -> index name i)
    where
      targetArr name (i,e) = Assign name i e

-} 

---------------------------------------------------------------------------
-- Also deal with pairs etc.. (future work)
---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- New Approach to Forceable. 
---------------------------------------------------------------------------
class Forceable a where
  type Forced a 
  force :: a -> Forced a


---------------------------------------------------------------------------
-- Force local
---------------------------------------------------------------------------
instance Scalar a => Forceable (Array Pull (Exp a)) where
  type Forced (Array Pull (Exp a)) = Program (Array Pull (Exp a))
  force arr = force (push arr) 

instance Scalar a => Forceable (Array Push (Exp a)) where
  type Forced (Array Push (Exp a)) = Program (Array Pull (Exp a)) 
  force (Array n (Push p)) =
    do
      -- Allocate is a bit strange since
      -- it wants the n in bytes! But also knows the type. 
    name <- Allocate (n * fromIntegral (sizeOf (undefined :: Exp a)))
                     (Pointer (typeOf (undefined :: (Exp a))))
    p (targetArr name)
    Sync
    return $ Array n $ Pull (\i -> index name i)
    where
      targetArr name (i,e) = Assign name i e

instance (Forceable a, Forceable b) => Forceable (a,b) where
  type Forced (a,b) = (Forced a, Forced b)
  force (a,b) = (force a, force b) 


instance (Forceable a, Forceable b, Forceable c) => Forceable (a,b,c) where
  type Forced (a,b,c) = (Forced a, Forced b, Forced c)
  force (a,b,c) = (force a, force b, force c) 

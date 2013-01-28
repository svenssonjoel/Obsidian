
{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             ScopedTypeVariables,
             TypeFamilies,
             GADTs #-} 

{- Joel Svensson 2012, 2013 

   Notes:

   2013-01-02: Added simple forceG for globArrays
   2012-12-10: Edited 


   TODO:
    # This is a pretty problematic module.
      Figure out how to generalise the force functions
      to things like Push arrays of pairs.. 
-}

--  write_ should be internal use only
module Obsidian.Force (Forceable, Forced, force,write_, forceG,forceG2) where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Array
import Obsidian.Types

import Data.Word
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
      name <- Allocate (n * fromIntegral (sizeOf (undefined :: Exp a)))
                        (Pointer (typeOf (undefined :: (Exp a))))
      p (targetArr name)
      -- BSync
      return $ Pull n (\i -> index name i)
    where
      targetArr name e i = Assign name i e

  force p =
    do
      rval <- write_ p
      Sync
      return rval

-- Is it possible to avoid being this repetitive ? 
instance (Scalar a,Scalar b) => Forceable (Push (Exp a,Exp b)) where
  type Forced (Push (Exp a,Exp b)) = Pull (Exp a, Exp b)
  write_ (Push n p) =
    do
      -- Allocate is a bit strange since
      -- it wants the n in bytes! But also knows the type. 
      name1 <- Allocate (n * fromIntegral (sizeOf (undefined :: Exp a)))
                         (Pointer (typeOf (undefined :: (Exp a))))
      name2 <- Allocate (n * fromIntegral (sizeOf (undefined :: Exp b)))
                         (Pointer (typeOf (undefined :: (Exp b))))
      p (targetArr (name1,name2))
      -- Sync
      return $  Pull n (\i -> (index name1 i,
                               index name2 i))
                     
    where
      targetArr (name1,name2) (e1,e2) i = Assign name1 i e1 >>
                                          Assign name2 i e2

  force p =
    do
      rval <- write_ p
      Sync
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
      Sync
      return rval


---------------------------------------------------------------------------
-- Global
---------------------------------------------------------------------------

-- TODO: Make typeclass! 
forceG :: forall a. Scalar a => GlobPush (Exp a)
           -> Final (GProgram (GlobPull (Exp a)))
forceG (GlobPush pbt) = Final $ 
  do
      global <- Output $ Pointer (typeOf (undefined :: Exp a))
      
      pbt (assignTo global)
        
      return $ GlobPull (\gix -> index global gix) 
    where 
      assignTo name e i = Assign name i e


forceG2 :: forall a b. (Scalar a, Scalar b) => (GlobPush (Exp a), GlobPush (Exp b))
           -> Final (GProgram (GlobPull (Exp a),GlobPull (Exp b)))
forceG2 (GlobPush pbt1,
         GlobPush pbt2) = Final $ 
  do
      global1 <- Output $ Pointer (typeOf (undefined :: Exp a))
      global2 <- Output $ Pointer (typeOf (undefined :: Exp b))
      
      pbt1 (assignTo global1) *||* pbt2 (assignTo global2)
        
      return (GlobPull (\gix -> index global1 gix),
              GlobPull (\gix -> index global2 gix) )
    where 
      assignTo name e i = Assign name i e





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

module Obsidian.Force (Forceable, Forced, force, forceG) where


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

-- Generalise ?
forceG :: forall a. Scalar a => GlobArray (Exp a)
           -> Final (GProgram (Distrib (Pull (Exp a))))
forceG (GPush nb bs pbt) = Final $ 
  do
      global <- Output $ Pointer (typeOf (undefined :: Exp a))
      
      pbt (assignTo global bs)
        
      return $ Distrib nb  $ 
        \bix -> (Pull bs (\ix -> index global ((bix * (fromIntegral bs)) + ix)))
    where 
      assignTo name s e b i = Assign name ((b*(fromIntegral s))+i) e
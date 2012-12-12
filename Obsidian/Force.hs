
{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             ScopedTypeVariables,
             TypeFamilies,
             GADTs #-} 

{- Joel Svensson 2012

   Notes:

   2012-12-10: Edited 


   TODO:
    # This is a pretty problematic module.
      Figure out how to generalise the force functions
      to things like Push arrays of pairs.. 
-}

module Obsidian.Force where


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


---------------------------------------------------------------------------
-- Assignment "structs"  (decompose pairs of pairs of ... of pairs in some way
---------------------------------------------------------------------------

class Structure a where
  type Repr a 
  struct :: a -> Struct (Repr a)
  deStruct :: Struct (Repr a) -> a 

data Struct a where
  Single :: Scalar a => Exp a -> Struct (Exp a) 
  SCons :: (Struct a, Struct b) -> Struct (a,b)


instance Scalar a => Structure (Exp a) where
  type Repr (Exp a) = Exp a 
  struct e = Single e
  deStruct (Single e) = e


instance (Structure a, Structure b) => Structure (a,b) where
  type Repr (a,b) = (Repr a, Repr b) 
  struct (a,b) = SCons (struct a, struct b) 
  deStruct (SCons (s1,s2)) = (deStruct s1, deStruct s2) 


instance (Structure a, Structure b, Structure c) => Structure (a,b,c) where
  type Repr (a,b,c) = (Repr a, Repr (b,c)) 
  struct (a,b,c) = SCons (struct a, SCons (struct b, struct c)) 
  deStruct (SCons (s1, SCons (s2,s3))) = (deStruct s1, deStruct s2, deStruct s3) 


---------------------------------------------------------------------------
-- Assign code for various structs. 
---------------------------------------------------------------------------


-- the "writefunction"
targetArray :: [String] -> Struct a -> Exp Word32 -> TProgram ()
targetArray noms s ix = snd $ targetArray' 0 noms s ix  


-- use a left to right numbering of the leaves of the three. 
targetArray' :: Int -> [String] -> Struct a -> Exp Word32 -> (Int,TProgram ())
targetArray' id noms (Single e1) ix = (id + 1,TAssign (noms !! id) ix e1)
targetArray' id noms (SCons  (s1,s2)) ix =
  let (id',prg) = targetArray' id noms s1 ix
      (id'',prg') = targetArray' id' noms s2 ix
  in (id'',prg >> prg')


-- To know how many names to allocate 
structSize :: Struct a -> Int
structSize (Single _) = 1
structSize (SCons (s1,s2)) = structSize s1 + structSize s2 

-- The types to allocate
structTypes :: Struct a -> [Type]
structTypes (Single e) = [typeOf e]
structTypes (SCons (s1,s2)) = structTypes s1 ++ structTypes s2

allocateAll :: Struct a -> Word32 -> BProgram [String]
allocateAll s nElts = mapM alloca types 
  where
    types = structTypes s
    alloca t = BAllocate (nElts * fromIntegral (typeSize t))
                         (Pointer t)
---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
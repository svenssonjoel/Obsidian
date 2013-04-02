{-# LANGUAGE ScopedTypeVariables #-}

{- Joel Svensson 2013 -} 

module Obsidian.Memory (MemoryOps(..),GlobalMemoryOps(..),assignArrayN)  where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Array -- Importing this feels a bit strange.
import Obsidian.Names

import Data.Word

class Inspect a where
  inspect :: a -> Tree (Either Name Type)

instance Scalar a => Inspect (Exp a) where
  inspect (Index (name,[])) = Single (Left name)
  inspect _ = Single $ Right (typeOf (undefined :: Exp a)) 

instance (Inspect a, Inspect b) => Inspect (a,b) where
  inspect (a,b) = Tuple [inspect a, inspect b]

instance (Inspect a, Inspect b, Inspect c) => Inspect (a,b,c) where
  inspect (a,b,c) = Tuple [inspect a, inspect b, inspect c]

  

---------------------------------------------------------------------------
-- Local Memory
---------------------------------------------------------------------------
class Inspect a => MemoryOps a where
  names          :: String -> a -> Program t Names
  allocateArray  :: Names -> a -> Word32 -> Program t ()
  allocateScalar :: Names -> a -> Program t () 
  assignArray    :: Names -> a -> Exp Word32 -> TProgram ()
  assignArrayS   :: Tree (Either Name Name) -> a -> Exp Word32 -> TProgram Names
  assignScalar   :: Names -> a -> TProgram () 
  pullFrom       :: Names -> Word32 -> Pull Word32 a
  readFrom       :: Names -> a

---------------------------------------------------------------------------
-- Derived
---------------------------------------------------------------------------
assignArrayN :: MemoryOps a =>
                Word32 -> a -> Exp Word32 -> TProgram Names
assignArrayN n a ix  =
  do
    names <- allocateNeeded n insp
    assignArrayS names a ix
    -- return names
  where
    insp = inspect a


allocateNeeded :: Word32 -> Tree (Either Name Type) -> TProgram (Tree (Either Name Name))
allocateNeeded n None = return None
allocateNeeded n a@(Single (Left nom)) = return $ Single (Left nom)
allocateNeeded n (Single (Right t)) =
  do
    name <- uniqueNamed "arr"
    Allocate name (n * typeSize t) (Pointer t)
    return $ Single (Right name)
allocateNeeded n (Tuple xs) =
  do
    ns <- mapM (allocateNeeded n) xs
    return $ Tuple ns 
    

---------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------
instance Scalar a => MemoryOps (Exp a) where
  names pre a = do {i <- uniqueNamed pre; return (Single i)}
  allocateArray (Single name) a n = 
    Allocate name (n * fromIntegral (sizeOf a))
                  (Pointer (typeOf a))
  allocateScalar (Single name) a =
    Declare name (typeOf a) 
  assignArray  (Single name) a ix = Assign name [ix] a
  assignArrayS (Single (Left name)) a ix = return $ Single name
  assignArrayS (Single (Right name)) a ix =
    do 
      Assign name [ix] a
      return $ Single name
    
  

  

  assignScalar (Single name) a    = Assign name [] a  
  pullFrom (Single name) n = Pull n (\i -> index name i) 
  readFrom (Single name) = variable name

instance (MemoryOps a, MemoryOps b) => MemoryOps (a, b) where
  names pre (a,b) =
    do
      a' <- names pre a
      b' <- names pre b
      return $ Tuple [a', b']
  allocateArray (Tuple [ns1,ns2]) (a,b) n =
    do 
      allocateArray ns1 a n
      allocateArray ns2 b n
  allocateScalar (Tuple [ns1,ns2]) (a,b) =
    do
      allocateScalar ns1 a
      allocateScalar ns2 b 
  assignArray (Tuple [ns1,ns2]) (a,b) ix =
    do
      assignArray ns1 a ix 
      assignArray ns2 b ix

  assignArrayS (Tuple [ns1,ns2]) (a,b) ix =
    do
      nas1 <- assignArrayS ns1 a ix 
      nas2 <- assignArrayS ns2 b ix
      return $ Tuple [nas1,nas2]
  assignScalar (Tuple [ns1,ns2]) (a,b) =
    do
      assignScalar ns1 a 
      assignScalar ns2 b  
  pullFrom (Tuple [ns1,ns2]) n =
    let p1 = pullFrom ns1 n
        p2 = pullFrom ns2 n
    in Pull n (\ix -> (p1 ! ix, p2 ! ix))
  readFrom (Tuple [ns1,ns2])  =
    let p1 = readFrom ns1
        p2 = readFrom ns2
    in (p1,p2)


---------------------------------------------------------------------------
-- Global Memory
---------------------------------------------------------------------------

class GlobalMemoryOps a where
  outputs   :: a -> GProgram Names
  assignOut :: Names -> a -> Exp Word32 -> Program Thread ()


instance Scalar a => GlobalMemoryOps (Exp a) where
  outputs a =
    do
      name <- Output $ Pointer $ typeOf a
      return (Single name) 
  assignOut (Single name) a ix = Assign name [ix] a


instance (GlobalMemoryOps a, GlobalMemoryOps b)
         => GlobalMemoryOps (a,b) where
  outputs (a,b) =
    do
      na <- outputs a
      nb <- outputs b
      return (Tuple [na,nb]) 
  assignOut (Tuple [n1,n2]) (a,b) ix =
    do
      assignOut n1 a ix 
      assignOut n2 b ix

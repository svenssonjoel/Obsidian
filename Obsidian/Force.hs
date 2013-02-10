
{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             ScopedTypeVariables,
             TypeFamilies,
             GADTs,
             OverlappingInstances,   -- REMOVE 
             IncoherentInstances #-} -- REMOVE 

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
module Obsidian.Force (write,
                       force,
                       --Forceable,
                       --Forced,
                       --force,
                       --write_,
                       forceG,
                       forceG2,
                       forceGP,
                       StoreOps) where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Array
import Obsidian.Types
import Obsidian.Globs

import Data.Word
---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------
data Names = Single Name
           | Tuple [Names]

class StoreOps a where
  names    :: a -> Program t Names 
  allocate :: Names -> a -> Word32 -> Program t ()
  assign   :: Names -> a -> Exp Word32 -> TProgram ()
  pullFrom :: Names -> Word32 -> Pull a


instance Scalar a => StoreOps (Exp a) where
  names a = do {i <- uniqueSM; return (Single i)}
  allocate (Single name) a n = 
      Allocate name (n * fromIntegral (sizeOf a))
                      (Pointer (typeOf a))
  assign (Single name) a ix = Assign name ix a  
  pullFrom (Single name) n = Pull n (\i -> index name i) 

instance (StoreOps a, StoreOps b) => StoreOps (a, b) where
  names (a,b) =
    do
      a' <- names a
      b' <- names b
      return $ Tuple [a', b']
  allocate (Tuple [ns1,ns2]) (a,b) n =
    do 
      allocate ns1 a n
      allocate ns2 b n
  assign (Tuple [ns1,ns2]) (a,b) ix =
    do
      assign ns1 a ix
      assign ns2 b ix
  pullFrom (Tuple [ns1,ns2]) n =
    let p1 = pullFrom ns1 n
        p2 = pullFrom ns2 n
    -- just a zip
    in Pull n (\ix -> (p1 ! ix, p2 ! ix))

---------------------------------------------------------------------------
-- New Approach to Forceable. 
---------------------------------------------------------------------------
--class Forceable a where
--  type Forced a 
--  write_ :: a -> BProgram (Forced a)
--  force  :: a -> BProgram (Forced a)  
  
---------------------------------------------------------------------------
-- Force local
---------------------------------------------------------------------------

write :: forall p a. (Len p, Pushable p, StoreOps a) => p a -> BProgram (Pull a)
write arr = do 
  snames <- names (undefined :: a)
  
  allocate snames (undefined :: a) (len arr) 

  let (Push m p) = push arr

  p (assign snames) 
      
  return $ pullFrom snames (len arr) 

  
force :: forall p a. (Len p, Pushable p, StoreOps a) =>  p a -> BProgram (Pull a)
force arr = do
  rval <- write arr
  Sync
  return rval
  
{- 
instance StoreOps a => Forceable (Pull a) where
  type Forced (Pull a) = Pull a
  write_ arr =
    do

      snames <- names (undefined :: a)

      allocate snames (undefined :: a) (len arr) 

      let (Push m p) = push arr

      p (assign snames) 
      
      return $ pullFrom snames (len arr) 
    

  force p = 
    do
      rval <- write_ p
      Sync
      return rval

  
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
      shared <- uniqueSM -- id <- Identifier 
      Allocate shared (n * fromIntegral (sizeOf (undefined :: Exp a)))
                        (Pointer (typeOf (undefined :: (Exp a))))
      p (targetArr shared)
      -- BSync
      return $ Pull n (\i -> index shared i)
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
      shared1 <- uniqueSM
      shared2 <- uniqueSM 
      Allocate shared1 (n * fromIntegral (sizeOf (undefined :: Exp a)))
                         (Pointer (typeOf (undefined :: (Exp a))))
      Allocate shared2 (n * fromIntegral (sizeOf (undefined :: Exp b)))
                         (Pointer (typeOf (undefined :: (Exp b))))
      p (targetArr (shared1,shared2))
      -- Sync
      return $  Pull n (\i -> (index shared1 i,
                               index shared2 i))
                     
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
-}

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


forceGP :: forall a. Scalar a => GlobPush (Exp a) -> GProgram ()
forceGP (GlobPush pbt) =
  do
    global <- Output $ Pointer (typeOf (undefined :: Exp a))

    pbt (assignTo global)

    return ()
  
    where
      assignTo name e i = Assign name i e 


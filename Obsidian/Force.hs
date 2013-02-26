
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
                       StoreOps(..),
                       forceG
                      ) where


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
  pullFrom :: Names -> Word32 -> Pull Word32 a


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
-- Force local (requires static lengths!) 
---------------------------------------------------------------------------

write :: forall p a. (Array p, Pushable p, StoreOps a) => p Word32 a -> BProgram (Pull Word32 a)
write arr = do 
  snames <- names (undefined :: a)

  -- Here i know that this pattern match will succeed
  let n = len arr
  
  allocate snames (undefined :: a) n

  let (Push m p) = push Block arr

  p (assign snames) 
      
  return $ pullFrom snames n

  
force :: forall p a. (Array p, Pushable p, StoreOps a) =>  p Word32 a -> BProgram (Pull Word32 a)
force arr = do
  rval <- write arr
  Sync
  return rval



-- Experimental forceG 
forceG :: forall a. Scalar a
          => Push Grid (Exp Word32) (Exp a)
          -> GProgram () -- Really to something else (named output ?)
forceG (Push _ p)  =
  do
    output <- Output $ Pointer (typeOf (undefined :: Exp a))
    p (assignTo output) 
    return ()
    where
      assignTo nom a ix = Assign nom ix a 
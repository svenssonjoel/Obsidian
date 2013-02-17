
{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             ScopedTypeVariables,
             TypeFamilies,
             GADTs  #-}

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
--                       forceG,
 --                      forceG2,
  --                     forceGP,
                       StoreOps) where


import Obsidian.Program
import Obsidian.Exp   hiding (Z)
import Obsidian.Array
import Obsidian.Shape 
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
  assign   :: Shapely sh => Names -> sh -> (E sh ,a) -> TProgram ()
  pullFrom :: Shapely sh => Names -> sh -> Pull b sh a


instance Scalar a => StoreOps (Exp a) where
  names a = do {i <- uniqueSM; return (Single i)}
  allocate (Single name) a n = 
      Allocate name (n * fromIntegral (sizeOf a))
                      (Pointer (typeOf a))
  assign (Single name) sh (ix,a) = Assign name (toIndex sh ix) a  
  pullFrom (Single name) sh = Pull sh (\i -> index name (toIndex sh i)) 

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
  assign (Tuple [ns1,ns2]) sh (ix,(a,b)) =
    do
      assign ns1 sh (ix,a)
      assign ns2 sh (ix,b)
  pullFrom (Tuple [ns1,ns2]) sh =
    let p1 = pullFrom ns1 sh
        p2 = pullFrom ns2 sh
    -- just a zip
    in Pull sh (\ix -> (p1 ! ix, p2 ! ix))

---------------------------------------------------------------------------
-- Force local
---------------------------------------------------------------------------

write :: forall p sh a. (Static  sh,    -- clean up these lists! 
                         Shapely sh, 
                         StoreOps a,
                         Pushable p Block sh,
                         Array p Block sh)
         => p Block sh a -> BProgram (Pull Block sh a)
write arr = do 
  snames <- names (undefined :: a)
  
  allocate snames (undefined :: a) (sizeS (shape arr)) 

  let (Push m p) = push arr
      
  p (assign snames (shape arr)) 
      
  return $ pullFrom snames (shape arr) 

  

force :: forall p sh a. (Static  sh, -- clean up these lists! 
                         Shapely sh,
                         StoreOps a,
                         Pushable p Block sh,
                         Array p Block sh)
         => p Block sh a -> BProgram (Pull Block sh a)
force arr = do
  rval <- write arr
  Sync
  return rval


  
---------------------------------------------------------------------------
-- Global
---------------------------------------------------------------------------
{- 
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

-} 

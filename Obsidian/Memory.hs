{-# LANGUAGE ScopedTypeVariables,
             GADTs #-}
{-# LANGUAGE FlexibleContexts #-}


{- Joel Svensson 2013

   This Module became quite messy.
   TODO: CLEAN IT UP! 

   notes: 2013-05-02: Cleaned out inspect. 

-} 

module Obsidian.Memory (Storable(..))  where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Array -- Importing this feels a bit strange.
import Obsidian.Names

import Obsidian.Dimension

import Data.Word

-- class MemoryOps a => Storable a 

---------------------------------------------------------------------------
-- Local Memory
---------------------------------------------------------------------------
class  Storable a where
  -- | Obtain new names for variables / arrays 
  names          :: String -> Program t (Names a) 

  -- Array operations 
  assignArray    :: Names a -> a -> Exp Word32 -> Program Thread ()
  allocateArray  :: Names a -> Dims d Word32 -> Program t ()
  pullFrom       :: Names a -> Static d -> Pull Static d a

  

  -- Scalar operations 
  assignScalar   :: Names a -> a -> Program Thread ()
  allocateScalar :: Names a ->  Program t () 
  readFrom       :: Names a -> a
  
  
  -- Warp level operations   
  warpAssignArray   :: Names a
                      -> EWord32
                      -> Word32
                      -> a
                      -> EWord32
                      -> Program Thread ()
  warpPullFrom      :: Num (Dims d EW32) => Names a -> Dynamic d -> Static d -> Pull Static d a
  
  -- Extra
  allocateVolatileArray :: Names a -> Dims d Word32 -> Program t ()




---------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------
instance Scalar a => Storable (Exp a) where

  -- Names 
  names pre = do {i <- uniqueNamed pre; return (Single i)}

  --Array ops 
  allocateArray (Single name) n = 
    Allocate name (size n * fromIntegral (sizeOf (undefined :: Exp a)))
                  (Pointer (typeOf (undefined :: Exp a)))
  assignArray  (Single name) a ix = Assign name [ix] a
  pullFrom (Single name) n = mkPull n (\i -> index name i)
  
  -- Scalar ops 
  allocateScalar (Single name) =
    Declare name (typeOf (undefined :: Exp a)) 
  assignScalar (Single name) a    = Assign name [] a
  readFrom  (Single name) = variable name

  -- Warp ops   
  warpAssignArray (Single name) warpID step a ix =
    Assign name [warpID * fromIntegral step + ix] a 

  warpPullFrom (Single name) warpID n
    = mkPull n (\i -> index name (toIndex (extents warpID * extents n + fromIndex i)))

  -- Extra 
  allocateVolatileArray (Single name) n = 
    Allocate name (size n * fromIntegral (sizeOf (undefined :: Exp a)))
                  (Volatile (Pointer (typeOf (undefined :: Exp a))))
  


instance (Storable a, Storable b) => Storable (a, b) where
  names pre =
    do
      (a' :: Names a) <- names pre
      (b' :: Names b) <- names pre 
      return $ Tuple a' b'
  allocateArray (Tuple ns1 ns2)  n =
      allocateArray ns1 n >> 
      allocateArray ns2 n
      
  allocateVolatileArray (Tuple ns1 ns2)  n =
      allocateVolatileArray ns1 n >> 
      allocateVolatileArray ns2 n

      
  allocateScalar (Tuple ns1 ns2) =
      allocateScalar ns1 >> 
      allocateScalar ns2
      
  assignArray (Tuple ns1 ns2) (a,b) ix =
      assignArray ns1 a ix >>
      assignArray ns2 b ix
      
  warpAssignArray (Tuple ns1 ns2) warpID step (a,b) ix =
      warpAssignArray ns1 warpID step a ix >>
      warpAssignArray ns2 warpID step b ix
      
  
  assignScalar (Tuple ns1 ns2) (a,b) =
      assignScalar ns1 a >>
      assignScalar ns2 b  

  pullFrom (Tuple ns1 ns2) n =
    let p1 = pullFrom ns1 n
        p2 = pullFrom ns2 n
    in mkPull n (\ix -> (p1 ! ix, p2 ! ix))
       
  warpPullFrom (Tuple ns1 ns2) warpID n
    = let p1 = warpPullFrom ns1 warpID n
          p2 = warpPullFrom ns2 warpID n
      in mkPull n (\ix -> (p1 ! ix, p2 ! ix)) 

  readFrom (Tuple ns1 ns2)  =
    let p1 = readFrom ns1
        p2 = readFrom ns2
    in (p1,p2)

  


instance (Storable a, Storable b, Storable c) => Storable (a, b, c) where
  names pre =
    do
      (a :: Names a) <- names pre 
      (b :: Names b) <- names pre 
      (c :: Names c) <- names pre 
      return $ Triple a b c
      
  allocateArray (Triple ns1 ns2 ns3) n =
      allocateArray ns1 n >>
      allocateArray ns2 n >> 
      allocateArray ns3 n
      
  allocateVolatileArray (Triple ns1 ns2 ns3) n =
      allocateVolatileArray ns1 n >> 
      allocateVolatileArray ns2 n >> 
      allocateVolatileArray ns3 n 
      
  allocateScalar (Triple ns1 ns2 ns3) =
      allocateScalar ns1 >>
      allocateScalar ns2 >>
      allocateScalar ns3
      
  assignArray (Triple ns1 ns2 ns3) (a,b,c) ix =
      assignArray ns1 a ix >>
      assignArray ns2 b ix >>
      assignArray ns3 c ix
      
  warpAssignArray (Triple ns1 ns2 ns3) warpID step (a,b,c) ix =
      warpAssignArray ns1 warpID step a ix >>
      warpAssignArray ns2 warpID step b ix >>
      warpAssignArray ns3 warpID step c ix 
  

  assignScalar (Triple ns1 ns2 ns3) (a,b,c) =
      assignScalar ns1 a >>
      assignScalar ns2 b >>
      assignScalar ns3 c
      
  pullFrom (Triple ns1 ns2 ns3) n =
    let p1 = pullFrom ns1 n
        p2 = pullFrom ns2 n
        p3 = pullFrom ns3 n
    in mkPull n (\ix -> (p1 ! ix, p2 ! ix,p3 ! ix))
       
  warpPullFrom (Triple ns1 ns2 ns3) warpID n
    = let p1 = warpPullFrom ns1 warpID n
          p2 = warpPullFrom ns2 warpID n
          p3 = warpPullFrom ns3 warpID n
      in mkPull n (\ix -> (p1 ! ix, p2 ! ix, p3 ! ix)) 

  readFrom (Triple ns1 ns2 ns3)  =
    let p1 = readFrom ns1
        p2 = readFrom ns2
        p3 = readFrom ns3
    in (p1,p2,p3)
 

{-# LANGUAGE ScopedTypeVariables,
             GADTs #-}

{- Joel Svensson 2013

   This Module became quite messy.
   TODO: CLEAN IT UP! 

   notes: 2013-05-02: Cleaned out inspect. 

-} 

module Obsidian.Memory (Storable(..))  where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Types
import Obsidian.Array -- Importing this feels a bit strange.
import Obsidian.Names

import Data.Word

-- class MemoryOps a => Storable a 

---------------------------------------------------------------------------
-- Local Memory
---------------------------------------------------------------------------
class Storable a where
  -- | Obtain new names for variables / arrays 
  names          :: String -> Program t (Names a) 

  -- Array operations 
  assignArray    :: Names a -> a -> Exp Word32 -> Program Thread ()
  allocateArray  :: Names a -> Word32 -> Program t ()
  pullFrom       :: ASize s => Names a -> s -> Pull s a

  
  -- Scalar operations 
  assignScalar   :: Names a -> a -> Program Thread ()
  allocateScalar :: Names a ->  Program t ()
  allocateSharedScalar :: Names a -> Program t () 
  readFrom       :: Names a -> a
  
  -- Warp level operations   
  warpAssignArray   :: Names a
                      -> EWord32
                      -> Word32
                      -> a
                      -> EWord32
                      -> Program Thread ()
  warpPullFrom      :: Names a -> EWord32 -> Word32 -> Pull Word32 a

  threadAssignArray :: Names a
                       -> EWord32
                       -> Word32
                       -> a
                       -> EWord32
                       -> Program Thread ()
  threadPullFrom :: Names a -> EWord32 -> Word32 -> Pull Word32 a 
  
  -- Extra
  allocateVolatileArray :: Names a -> Word32 -> Program t ()




---------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------
instance Scalar a => Storable (Exp a) where

  -- Names 
  names pre = do {i <- uniqueNamed pre; return (Single i)}

  --Array ops 
  allocateArray (Single name) n = 
    Allocate name (n * fromIntegral (sizeOf (undefined :: Exp a)))
                  (Pointer (typeOf (undefined :: Exp a)))
  assignArray  (Single name) a ix = Assign name [ix] a
  pullFrom (Single name) n = mkPull n (\i -> index name i)
  
  -- Scalar ops 
  allocateScalar (Single name) =
    Declare name (typeOf (undefined :: Exp a))
  allocateSharedScalar (Single name) =
    Declare name (Shared $ typeOf (undefined :: Exp a))
  
  assignScalar (Single name) a    = Assign name [] a
  readFrom  (Single name) = variable name

  -- Warp ops   
  warpAssignArray (Single name) warpId step a ix =
    Assign name [warpId * fromIntegral step + ix] a 

  warpPullFrom (Single name) warpId n
    = mkPull n (\i -> index name (warpId * fromIntegral n + i))

  -- Thread ops
  threadAssignArray (Single name) threadId step a ix =
    Assign name [threadId * fromIntegral step + ix] a

  threadPullFrom (Single name) threadId n
    = mkPull n (\i -> index name (threadId * fromIntegral n + i)) 

  -- Extra 
  allocateVolatileArray (Single name) n = 
    Allocate name (n * fromIntegral (sizeOf (undefined :: Exp a)))
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

  allocateSharedScalar (Tuple ns1 ns2) =
      allocateSharedScalar ns1 >> 
      allocateSharedScalar ns2
  
      
  assignArray (Tuple ns1 ns2) (a,b) ix =
      assignArray ns1 a ix >>
      assignArray ns2 b ix
      
  warpAssignArray (Tuple ns1 ns2) warpID step (a,b) ix =
      warpAssignArray ns1 warpID step a ix >>
      warpAssignArray ns2 warpID step b ix

  threadAssignArray (Tuple ns1 ns2) threadId step (a,b) ix =
    threadAssignArray ns1 threadId step a ix >> 
    threadAssignArray ns2 threadId step b ix 
      
  
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

  threadPullFrom (Tuple ns1 ns2) threadId n
    = let p1 = threadPullFrom ns1 threadId n
          p2 = threadPullFrom ns2 threadId n
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

  allocateSharedScalar (Triple ns1 ns2 ns3) =
      allocateSharedScalar ns1 >>
      allocateSharedScalar ns2 >>
      allocateSharedScalar ns3


  assignArray (Triple ns1 ns2 ns3) (a,b,c) ix =
      assignArray ns1 a ix >>
      assignArray ns2 b ix >>
      assignArray ns3 c ix
      
  warpAssignArray (Triple ns1 ns2 ns3) warpID step (a,b,c) ix =
      warpAssignArray ns1 warpID step a ix >>
      warpAssignArray ns2 warpID step b ix >>
      warpAssignArray ns3 warpID step c ix

  threadAssignArray (Triple ns1 ns2 ns3) threadID step (a,b,c) ix =
    threadAssignArray ns1 threadID step a ix >>
    threadAssignArray ns2 threadID step b ix >>
    threadAssignArray ns3 threadID step c ix


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

  threadPullFrom (Triple ns1 ns2 ns3) threadId n
    = let p1 = threadPullFrom ns1 threadId n
          p2 = threadPullFrom ns2 threadId n
          p3 = threadPullFrom ns3 threadId n 
      in mkPull n (\ix -> (p1 ! ix, p2 ! ix,p3 ! ix))


  readFrom (Triple ns1 ns2 ns3)  =
    let p1 = readFrom ns1
        p2 = readFrom ns2
        p3 = readFrom ns3
    in (p1,p2,p3)
 
instance (Storable a, Storable b, Storable c, Storable d) => Storable (a, b, c, d) where
  names pre =
    do
      (a :: Names a) <- names pre
      (b :: Names b) <- names pre
      (c :: Names c) <- names pre
      (d :: Names d) <- names pre
      return $ Quadruple a b c d

  allocateArray (Quadruple ns1 ns2 ns3 ns4) n =
      allocateArray ns1 n >>
      allocateArray ns2 n >>
      allocateArray ns3 n >>
      allocateArray ns4 n

  allocateVolatileArray (Quadruple ns1 ns2 ns3 ns4) n =
      allocateVolatileArray ns1 n >>
      allocateVolatileArray ns2 n >>
      allocateVolatileArray ns3 n >>
      allocateVolatileArray ns4 n

  allocateScalar (Quadruple ns1 ns2 ns3 ns4) =
      allocateScalar ns1 >>
      allocateScalar ns2 >>
      allocateScalar ns3 >>
      allocateScalar ns4

  allocateSharedScalar (Quadruple ns1 ns2 ns3 ns4) =
      allocateSharedScalar ns1 >>
      allocateSharedScalar ns2 >>
      allocateSharedScalar ns3 >>
      allocateSharedScalar ns4


  assignArray (Quadruple ns1 ns2 ns3 ns4) (a,b,c,d) ix =
      assignArray ns1 a ix >>
      assignArray ns2 b ix >>
      assignArray ns3 c ix >>
      assignArray ns4 d ix

  warpAssignArray (Quadruple ns1 ns2 ns3 ns4) warpID step (a,b,c,d) ix =
      warpAssignArray ns1 warpID step a ix >>
      warpAssignArray ns2 warpID step b ix >>
      warpAssignArray ns3 warpID step c ix >>
      warpAssignArray ns4 warpID step d ix

  threadAssignArray (Quadruple ns1 ns2 ns3 ns4) threadID step (a,b,c,d) ix =
    threadAssignArray ns1 threadID step a ix >>
    threadAssignArray ns2 threadID step b ix >>
    threadAssignArray ns3 threadID step c ix >>
    threadAssignArray ns4 threadID step d ix
  

  assignScalar (Quadruple ns1 ns2 ns3 ns4) (a,b,c,d) =
      assignScalar ns1 a >>
      assignScalar ns2 b >>
      assignScalar ns3 c >>
      assignScalar ns4 d

  pullFrom (Quadruple ns1 ns2 ns3 ns4) n =
    let p1 = pullFrom ns1 n
        p2 = pullFrom ns2 n
        p3 = pullFrom ns3 n
        p4 = pullFrom ns4 n
    in mkPull n (\ix -> (p1 ! ix, p2 ! ix, p3 ! ix, p4 ! ix))

  warpPullFrom (Quadruple ns1 ns2 ns3 ns4) warpID n
   = let p1 = warpPullFrom ns1 warpID n
         p2 = warpPullFrom ns2 warpID n
         p3 = warpPullFrom ns3 warpID n
         p4 = warpPullFrom ns4 warpID n
      in mkPull n (\ix -> (p1 ! ix, p2 ! ix, p3 ! ix, p4 ! ix))

  threadPullFrom (Quadruple ns1 ns2 ns3 ns4) threadID n
   = let p1 = threadPullFrom ns1 threadID n
         p2 = threadPullFrom ns2 threadID n
         p3 = threadPullFrom ns3 threadID n
         p4 = threadPullFrom ns4 threadID n
      in mkPull n (\ix -> (p1 ! ix, p2 ! ix, p3 ! ix, p4 ! ix))

  readFrom (Quadruple ns1 ns2 ns3 ns4)  =
    let p1 = readFrom ns1
        p2 = readFrom ns2
        p3 = readFrom ns3
        p4 = readFrom ns4
    in (p1,p2,p3,p4)

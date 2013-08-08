{-# LANGUAGE ScopedTypeVariables,
             GADTs #-}

{- Joel Svensson 2013

   This Module became quite messy.
   TODO: CLEAN IT UP! 

   notes: 2013-05-02: Cleaned out inspect. 

-} 

module Obsidian.Memory (MemoryOps(..))  where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Array -- Importing this feels a bit strange.
import Obsidian.Names

import Data.Word


---------------------------------------------------------------------------
-- Local Memory
---------------------------------------------------------------------------
class MemoryOps a where
  moNames          :: String -> Program t (Names a) 
  moAllocateArray  :: Names a -> Word32 -> Program Block ()
  moAllocateScalar :: Names a -> Program t () 
  moAssignArray    :: Names a -> a -> Exp Word32 -> TProgram ()
  moAssignScalar   :: Names a -> a -> TProgram () 
  moPullFrom       :: Names a -> Word32 -> Pull Word32 a
  moReadFrom       :: Names a -> a


---------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------
instance Scalar a => MemoryOps (Exp a) where
  moNames pre = do {i <- uniqueNamed pre; return (Single i)}
  moAllocateArray (Single name) n = 
    Allocate name (n * fromIntegral (sizeOf (undefined :: Exp a)))
                  (Pointer (typeOf (undefined :: Exp a)))
  moAllocateScalar (Single name) =
    Declare name (typeOf (undefined :: Exp a)) 
  moAssignArray  (Single name) a ix = Assign name [ix] a

  moAssignScalar (Single name) a    = Assign name [] a  
  moPullFrom (Single name) n = mkPull n (\i -> index name i) 
  moReadFrom  (Single name) = variable name


instance (MemoryOps a, MemoryOps b) => MemoryOps (a, b) where
  moNames pre {-(a,b)-} =
    do
      (a' :: Names a) <- moNames pre --a
      (b' :: Names b) <- moNames pre --b
      return $ Tuple a' b'
  moAllocateArray (Tuple ns1 ns2) {-(a,b)-} n =
    do 
      moAllocateArray ns1 {-a-} n
      moAllocateArray ns2 {-b-} n
  moAllocateScalar (Tuple ns1 ns2) {-(a,b)-} =
    do
      moAllocateScalar ns1 {-a-}
      moAllocateScalar ns2 {-b-} 
  moAssignArray (Tuple ns1 ns2) (a,b) ix =
    do
      moAssignArray ns1 a ix 
      moAssignArray ns2 b ix

  moAssignScalar (Tuple ns1 ns2) (a,b) =
    do
      moAssignScalar ns1 a 
      moAssignScalar ns2 b  

  moPullFrom (Tuple ns1 ns2) n =
    let p1 = moPullFrom ns1 n
        p2 = moPullFrom ns2 n
    in mkPull n (\ix -> (p1 ! ix, p2 ! ix))
  moReadFrom (Tuple ns1 ns2)  =
    let p1 = moReadFrom ns1
        p2 = moReadFrom ns2
    in (p1,p2)


instance (MemoryOps a, MemoryOps b, MemoryOps c) => MemoryOps (a, b, c) where
  moNames pre {-(a,b)-} =
    do
      (a :: Names a) <- moNames pre --a
      (b :: Names b) <- moNames pre --b
      (c :: Names c) <- moNames pre --b
      return $ Triple a b c
  moAllocateArray (Triple ns1 ns2 ns3) {-(a,b)-} n =
    do 
      moAllocateArray ns1 {-a-} n
      moAllocateArray ns2 {-b-} n
      moAllocateArray ns3 {-b-} n
  moAllocateScalar (Triple ns1 ns2 ns3) {-(a,b)-} =
    do
      moAllocateScalar ns1 {-a-}
      moAllocateScalar ns2 {-b-}
      moAllocateScalar ns3 {-b-} 
  moAssignArray (Triple ns1 ns2 ns3) (a,b,c) ix =
    do
      moAssignArray ns1 a ix 
      moAssignArray ns2 b ix
      moAssignArray ns3 c ix

  moAssignScalar (Triple ns1 ns2 ns3) (a,b,c) =
    do
      moAssignScalar ns1 a 
      moAssignScalar ns2 b
      moAssignScalar ns3 c
      
  moPullFrom (Triple ns1 ns2 ns3) n =
    let p1 = moPullFrom ns1 n
        p2 = moPullFrom ns2 n
        p3 = moPullFrom ns3 n
    in mkPull n (\ix -> (p1 ! ix, p2 ! ix,p3 ! ix))
  moReadFrom (Triple ns1 ns2 ns3)  =
    let p1 = moReadFrom ns1
        p2 = moReadFrom ns2
        p3 = moReadFrom ns3
    in (p1,p2,p3)
 

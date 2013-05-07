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
  names          :: String -> Program t (Names a) 
  allocateArray  :: Names a -> Word32 -> Program t ()
  allocateScalar :: Names a -> Program t () 
  assignArray    :: Names a -> a -> Exp Word32 -> TProgram ()
  assignScalar   :: Names a -> a -> TProgram () 
  pullFrom       :: Names a -> Word32 -> Pull Word32 a
  readFrom       :: Names a -> a


---------------------------------------------------------------------------
-- Instances
---------------------------------------------------------------------------
instance Scalar a => MemoryOps (Exp a) where
  names pre = do {i <- uniqueNamed pre; return (Single i)}
  allocateArray (Single name) n = 
    Allocate name (n * fromIntegral (sizeOf (undefined :: Exp a)))
                  (Pointer (typeOf (undefined :: Exp a)))
  allocateScalar (Single name) =
    Declare name (typeOf (undefined :: Exp a)) 
  assignArray  (Single name) a ix = Assign name [ix] a

  assignScalar (Single name) a    = Assign name [] a  
  pullFrom (Single name) n = Pull n (\i -> index name i) 
  readFrom (Single name) = variable name


instance (MemoryOps a, MemoryOps b) => MemoryOps (a, b) where
  names pre {-(a,b)-} =
    do
      (a' :: Names a) <- names pre --a
      (b' :: Names b) <- names pre --b
      return $ Tuple a' b'
  allocateArray (Tuple ns1 ns2) {-(a,b)-} n =
    do 
      allocateArray ns1 {-a-} n
      allocateArray ns2 {-b-} n
  allocateScalar (Tuple ns1 ns2) {-(a,b)-} =
    do
      allocateScalar ns1 {-a-}
      allocateScalar ns2 {-b-} 
  assignArray (Tuple ns1 ns2) (a,b) ix =
    do
      assignArray ns1 a ix 
      assignArray ns2 b ix

  assignScalar (Tuple ns1 ns2) (a,b) =
    do
      assignScalar ns1 a 
      assignScalar ns2 b  
  pullFrom (Tuple ns1 ns2) n =
    let p1 = pullFrom ns1 n
        p2 = pullFrom ns2 n
    in Pull n (\ix -> (p1 ! ix, p2 ! ix))
  readFrom (Tuple ns1 ns2)  =
    let p1 = readFrom ns1
        p2 = readFrom ns2
    in (p1,p2)


instance (MemoryOps a, MemoryOps b, MemoryOps c) => MemoryOps (a, b, c) where
  names pre {-(a,b)-} =
    do
      (a :: Names a) <- names pre --a
      (b :: Names b) <- names pre --b
      (c :: Names c) <- names pre --b
      return $ Triple a b c
  allocateArray (Triple ns1 ns2 ns3) {-(a,b)-} n =
    do 
      allocateArray ns1 {-a-} n
      allocateArray ns2 {-b-} n
      allocateArray ns3 {-b-} n
  allocateScalar (Triple ns1 ns2 ns3) {-(a,b)-} =
    do
      allocateScalar ns1 {-a-}
      allocateScalar ns2 {-b-}
      allocateScalar ns3 {-b-} 
  assignArray (Triple ns1 ns2 ns3) (a,b,c) ix =
    do
      assignArray ns1 a ix 
      assignArray ns2 b ix
      assignArray ns3 c ix

  assignScalar (Triple ns1 ns2 ns3) (a,b,c) =
    do
      assignScalar ns1 a 
      assignScalar ns2 b
      assignScalar ns3 c
      
  pullFrom (Triple ns1 ns2 ns3) n =
    let p1 = pullFrom ns1 n
        p2 = pullFrom ns2 n
        p3 = pullFrom ns3 n
    in Pull n (\ix -> (p1 ! ix, p2 ! ix,p3 ! ix))
  readFrom (Triple ns1 ns2 ns3)  =
    let p1 = readFrom ns1
        p2 = readFrom ns2
        p3 = readFrom ns3
    in (p1,p2,p3)
 
---------------------------------------------------------------------------
-- Global Memory
---------------------------------------------------------------------------

{- 
class GlobalMemoryOps a where
  -- outputs   :: a -> GProgram Names
  assignOut :: a -> Exp Word32 -> Program Thread ()


instance Scalar a => GlobalMemoryOps (Exp a) where
  --outputs a =
  --  do
  --    name <- Output $ Pointer $ typeOf a
  --    return (Single name) 
  assignOut  a ix =
    do
      name <- Output $ Pointer $ typeOf a
      Assign name [ix] a


instance (GlobalMemoryOps a, GlobalMemoryOps b)
         => GlobalMemoryOps (a,b) where
  --outputs (a,b) =
  --  do
  --    na <- outputs a
  --    nb <- outputs b
  --    return (Tuple [na,nb]) 
  assignOut (a,b) ix =
    do
      assignOut a ix 
      assignOut b ix
-}

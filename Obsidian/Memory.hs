
{- Joel Svensson 2013 -} 

module Obsidian.Memory (MemoryOps(..))  where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Array -- Importing this feels a bit strange. 

import Data.Word

data Names = Single Name
           | Tuple [Names]


class MemoryOps a where
  names          :: a -> Program t Names 
  allocateArray  :: Names -> a -> Word32 -> Program t ()
  allocateScalar :: Names -> a -> Program t () 
  assignArray    :: Names -> a -> Exp Word32 -> TProgram ()
  assignScalar   :: Names -> a -> TProgram () 
  pullFrom       :: Names -> Word32 -> Pull Word32 a
  readFrom       :: Names -> a


instance Scalar a => MemoryOps (Exp a) where
  names a = do {i <- uniqueSM; return (Single i)}
  allocateArray (Single name) a n = 
    Allocate name (n * fromIntegral (sizeOf a))
                  (Pointer (typeOf a))
  allocateScalar (Single name) a =
    Declare name (typeOf a) 
  assignArray  (Single name) a ix = Assign name [ix] a
  assignScalar (Single name) a    = Assign name [] a  
  pullFrom (Single name) n = Pull n (\i -> index name i) 
  readFrom (Single name) = variable name

instance (MemoryOps a, MemoryOps b) => MemoryOps (a, b) where
  names (a,b) =
    do
      a' <- names a
      b' <- names b
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
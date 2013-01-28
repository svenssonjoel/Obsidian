{- Joel Svensson 2012

   Notes:
   2013-01-08: removed number-of-blocks field from ForAllBlocks

-}

{-# LANGUAGE GADTs,
             FlexibleInstances #-} 


module Obsidian.Program  where 
 
import Data.Word
import Data.Monoid

import Obsidian.Exp
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Atomic

-- Package value-supply
import Data.Supply
import System.IO.Unsafe

---------------------------------------------------------------------------
-- Thread/Block/Grid 
---------------------------------------------------------------------------
data Thread
data Block
data Grid 

---------------------------------------------------------------------------
-- Program datatype
---------------------------------------------------------------------------
data Program t a where 
  Assign :: Scalar a
            => Name
            -> (Exp Word32)
            -> (Exp a)
            -> Program Thread ()
           
            
  AtomicOp :: Scalar a
              => Name 
              -> Exp Word32
              -> Atomic a
              -> Program Thread (Exp a)

  -- TODO: Code generation for this.
  -- May want this at all levels. (Grid, Block, Thread)
  Cond :: Exp Bool
          -> Program Thread ()
          -> Program Thread ()
  
  -- DONE: Code generation for this.            
  SeqFor :: Exp Word32 -> (Exp Word32 -> Program Thread ())
            -> Program Thread () 

  ForAll :: (Maybe Word32) 
            -> (Exp Word32 -> Program Thread ())
            -> Program Block () 

  {-
     I'm not sure about this constructor.
     As I see it programs from which we generate a kernel
     must be wrapped in one of these ForAllBlocks.
     Programs with sequences of 'ForAllBlocks' are problematic.

     Maybe a (ForAllBlocks n f *>* ForAllBlocks m g) Program
     should be split into two kernels. 
  -} 
  ForAllBlocks :: (Exp Word32 -> Program Block ()) 
                  -> Program Grid () 
  
  Allocate :: Word32 -> Type -> Program Block Name
  
  -- Very experimental AllocateG (Add CodeGen support)
  AllocateG :: Exp Word32 -> Type -> Program Grid Name 
  -- I'm not sure exactly what scope AllocateG should have. 

  {- About Output (Creates a named output array). 
     This is similar to Allocate but concerning global arrays.

     Since we cannot synchronize writes to a global array inside of an
     kernel, global arrays will only be written as outputs of the kernel
  -} 
  Output   :: Type -> Program Grid Name
  -- (Output may be replaced by AllocateG) 
  
  Sync     :: Program Block ()
  -- Two very experimental threadfence constructs.
  -- should correspond to cuda __threadfence();
  -- and __threadfenceBlock(); 
  ThreadFence :: Program Grid ()
  ThreadFenceBlock :: Program Block () 

  -- Parallel composition of Programs
  Par :: Program p () ->
         Program p () ->
         Program p () 

  -- Monad
  Return :: a -> Program t a
  Bind   :: Program t a -> (a -> Program t b) -> Program t b

---------------------------------------------------------------------------
-- forAll and forAllN
---------------------------------------------------------------------------
forAll :: (Exp Word32 -> Program Thread ()) -> Program Block () 
forAll f = ForAll Nothing f

forAllN :: Word32 -> (Exp Word32 -> Program Thread ()) -> Program Block ()
forAllN n f = ForAll (Just n) f

(*||*) = Par

---------------------------------------------------------------------------
-- forAllT
--------------------------------------------------------------------------- 
-- When we know that all threads are independent and
-- independent of "blocksize".
-- Also any allocation of local storage is impossible.
-- Composition of something using forAllT with something
-- that performs local computations is impossible.
-- Using the hardcoded BlockDim may turn out to be a problem when
-- we want to compute more than one thing per thread (may be fine though). 
forAllT :: (Exp Word32 -> Program Thread ())
           -> Program Grid ()
forAllT f = ForAllBlocks
            $ \bid -> ForAll Nothing
                      $ \tid -> f (bid * BlockDim X + tid) 

            

---------------------------------------------------------------------------
-- Monad
--------------------------------------------------------------------------
instance Monad (Program t) where
  return = Return
  (>>=) = Bind

---------------------------------------------------------------------------
-- Aliases 
---------------------------------------------------------------------------
type TProgram = Program Thread
type BProgram = Program Block
type GProgram = Program Grid 

---------------------------------------------------------------------------
-- runPrg 
---------------------------------------------------------------------------
runPrg :: Int -> Program t a -> (a,Int) 
runPrg i (Return a) = (a,i)
runPrg i (Bind m f) =
  let (a,i') = runPrg i m
  in runPrg i' (f a) 
runPrg i (Sync) = ((),i)
runPrg i (ForAll n ixf) =
  let (p,i') = runPrg i (ixf (variable "tid")) 
  in  (p,i') 
runPrg i (Allocate _ _) = ("new" ++ show i,i+1)
runPrg i (Assign _ _ a) = ((),i) -- Probaby wrong.. 
runPrg i (AtomicOp _ _ _) = (variable ("new"++show i),i+1)
     
---------------------------------------------------------------------------
-- printPrg
---------------------------------------------------------------------------
printPrg prg = (\(_,x,_) -> x) $ printPrg' 0 prg

printPrg' :: Int -> Program t a -> (a,String,Int)  
-- printPrg' i Skip = ((),";\n", i)
printPrg' i (Assign n ix e) =
  ((),n ++ "[" ++ show ix ++ "] = " ++ show e ++ ";\n", i) 
printPrg' i (AtomicOp n ix e) =
  let newname = "r" ++ show i
  in (variable newname,
      newname ++ " = " ++ printAtomic e ++
      "( " ++ n ++ "[" ++ show ix ++ "])\n",i+1)
printPrg' i (Allocate n t) =
  let newname = "arr" ++ show i
  in (newname,newname ++ " = malloc(" ++ show n ++ ");\n",i+1)
printPrg' i (Output t) =
  let newname = "globalOut" ++ show i
  in (newname,newname ++ " = new Global output;\n",i+1)
printPrg' i (ForAll n f) =
  let ((),prg2,i') = printPrg' i (f (variable "i"))
      
  in ( (),  
       "par (i in 0.." ++ show n ++ ")" ++
       "{\n" ++ prg2 ++ "\n}",
       i')
printPrg' i (ForAllBlocks f) =
  let (d,prg2,i') = printPrg' i (f (variable "BIX"))
  in ((), 
      "blocks (i)" ++
      "{\n" ++ prg2 ++ "\n}",
      i')
printPrg' i (Return a) = (a,"MonadReturn;\n",i)
printPrg' i (Bind m f) =
  let (a1, str1,i1) = printPrg' i m
      (a2,str2,i2) = printPrg' i1 (f a1)
  in (a2,str1 ++ str2, i2)
printPrg' i Sync = ((),"Sync;\n",i)


{- Joel Svensson 2012,2013

   Notes:
   2013-04-02: Added a Break statement to the language.
               Use it to break out of sequential loops.
   2013-01-08: removed number-of-blocks field from ForAllBlocks

-}

{-# LANGUAGE GADTs,
             FlexibleInstances,
             EmptyDataDecls #-} 


module Obsidian.Program  where 
 
import Data.Word
import Data.Monoid

import Obsidian.Exp
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Atomic
import Obsidian.Names

-- Package value-supply
import Data.Supply
import System.IO.Unsafe

---------------------------------------------------------------------------
-- Thread/Block/Grid 
---------------------------------------------------------------------------
data Thread
data Block
data Grid 

data PT a where
  Thread :: PT Thread
  Block  :: PT Block
  Grid   :: PT Grid 

type Identifier = Int 

---------------------------------------------------------------------------
-- Obsidian
---------------------------------------------------------------------------
data Obsidian a = Obsidian (Program Grid a)
      

---------------------------------------------------------------------------
-- Program datatype
--------------------------------------------------------------------------
data Program t a where
  
  Identifier :: Program t Identifier 
  
  Assign :: Scalar a
            => Name
            -> [Exp Word32]
            -> (Exp a)
            -> Program Thread ()
           
            
  AtomicOp :: Scalar a
              => Name 
              -> Exp Word32
              -> Atomic a
              -> Program Thread (Exp a)

  Cond :: Exp Bool
          -> Program t ()
          -> Program t ()
  
  -- DONE: Code generation for this.
  -- TODO: Generalize this loop! (Replace Thread with t) 
  SeqFor :: Exp Word32 -> (Exp Word32 -> Program t a)
            -> Program t a
  Break  :: Program Thread () 
 
  ForAll :: (Exp Word32) 
            -> (Exp Word32 -> Program Thread a)
            -> Program Block a 

  {-
     I'm not sure about this constructor.
     As I see it programs from which we generate a kernel
     must be wrapped in one of these ForAllBlocks.
     Programs with sequences of 'ForAllBlocks' are problematic.

     Maybe a (ForAllBlocks n f *>* ForAllBlocks m g) Program
     should be split into two kernels. 
  -} 
  ForAllBlocks :: (Exp Word32) -> (Exp Word32 -> Program Block a) 
                  -> Program Grid a

  ForAllThreads :: (Exp Word32) -> (Exp Word32 -> Program Thread a)
                   -> Program Grid a 

  -- Allocate shared memory in each MP

  -- TODO: Change the Liveness analysis to a two-pass algo
  --       and remove the Allocate constructor. 
  Allocate :: Name -> Word32 -> Type -> Program t () 

  -- Automatic Variables
  Declare :: Name -> Type -> Program t () 
              
  {- About Output (Creates a named output array). 
     This is similar to Allocate but concerning global arrays.

     Since we cannot synchronize writes to a global array inside of an
     kernel, global arrays will only be written as outputs of the kernel
  -}

  -- TODO: REMOVE THIS FROM THE AST,
  -- Discover the outputs from the "Type" when doing a ToProgram
  Output   :: Type -> Program t Name
  -- (Output may be replaced by AllocateG) 
  
  Sync     :: Program Block ()
  -- Two very experimental threadfence constructs.
  -- should correspond to cuda __threadfence();
  -- and __threadfenceBlock(); 
  ThreadFence :: Program Grid ()
  ThreadFenceBlock :: Program Block () 

  -- Parallel composition of Programs
  -- TODO: Will I use this ? 
  Par :: Program p () ->
         Program p () ->
         Program p () 

  -- Monad
  Return :: a -> Program t a
  Bind   :: Program t a -> (a -> Program t b) -> Program t b

---------------------------------------------------------------------------
-- Helpers 
--------------------------------------------------------------------------- 
uniqueSM = do
  id <- Identifier
  return $ "arr" ++ show id 

uniqueNamed pre = do
  id <- Identifier
  return $ pre ++ show id 

---------------------------------------------------------------------------
-- forAll and forAllN
---------------------------------------------------------------------------
--forAll :: (Exp Word32 -> Program Thread ()) -> Program Block () 
--forAll f = ForAll Nothing f

forAll :: Exp Word32 -> (Exp Word32 -> Program Thread a) -> Program Block a
forAll n f = ForAll n f

(*||*) = Par

---------------------------------------------------------------------------
-- SeqFor
---------------------------------------------------------------------------
seqFor :: Exp Word32 -> (Exp Word32 -> Program t a)
            -> Program t a
seqFor (Literal 1) f = f 0
seqFor n f = SeqFor n f


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
forAllT :: (Exp Word32) -> (Exp Word32 -> Program Thread a)
           -> Program Grid a
forAllT n f = ForAllThreads n 
            $ \gtid -> f gtid 



forAllBlocks = ForAllBlocks

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
-- runPrg (RETHINK!) 
---------------------------------------------------------------------------
runPrg :: Int -> Program t a -> (a,Int)
runPrg i Identifier = (i,i+1)
runPrg i (Return a) = (a,i)
runPrg i (Bind m f) =
  let (a,i') = runPrg i m
  in runPrg i' (f a) 
runPrg i (Sync) = ((),i)
runPrg i (ForAll n ixf) =
  let (p,i') = runPrg i (ixf (variable "tid")) 
  in  (p,i') 
runPrg i (Allocate id _ _ ) = ((),i)
runPrg i (Assign _ _ a) = ((),i) -- Probaby wrong.. 
runPrg i (AtomicOp _ _ _) = (variable ("new"++show i),i+1)
     
---------------------------------------------------------------------------
-- printPrg (REIMPLEMENT) xs
---------------------------------------------------------------------------
printPrg prg = (\(_,x,_) -> x) $ printPrg' 0 prg

printPrg' :: Int -> Program t a -> (a,String,Int)
printPrg' i Identifier = (i,"getId;\n",i+1) 
-- printPrg' i Skip = ((),";\n", i)
printPrg' i (Assign n ix e) =
  ((),n ++ "[" ++ show ix ++ "] = " ++ show e ++ ";\n", i) 
printPrg' i (AtomicOp n ix e) =
  let newname = "r" ++ show i
  in (variable newname,
      newname ++ " = " ++ printAtomic e ++
      "( " ++ n ++ "[" ++ show ix ++ "])\n",i+1)
printPrg' i (Allocate id n t) =
  let newname = id -- "arr" ++ show id
  in ((),newname ++ " = malloc(" ++ show n ++ ");\n",i+1)
printPrg' i (Declare id t) =
  let newname = id -- "arr" ++ show id
  in ((),show t ++ " " ++ newname ++ "\n",i+1)
printPrg' i (Output t) =
  let newname = "globalOut" ++ show i
  in (newname,newname ++ " = new Global output;\n",i+1)
printPrg' i (SeqFor n f) =
  let (a,prg2,i') = printPrg' i (f (variable "i"))
      
  in ( a,  
       "for (i in 0.." ++ show n ++ ")" ++
       "{\n" ++ prg2 ++ "\n}",
       i')
     
printPrg' i (ForAll n f) =
  let (a,prg2,i') = printPrg' i (f (variable "i"))
      
  in ( a,  
       "par (i in 0.." ++ show n ++ ")" ++
       "{\n" ++ prg2 ++ "\n}",
       i')
printPrg' i (ForAllBlocks n f) =
  let (d,prg2,i') = printPrg' i (f (variable "BIX"))
  in (d, 
      "blocks (i)" ++
      "{\n" ++ prg2 ++ "\n}",
      i')
printPrg' i (Return a) = (a,"MonadReturn;\n",i)
printPrg' i (Bind m f) =
  let (a1, str1,i1) = printPrg' i m
      (a2,str2,i2) = printPrg' i1 (f a1)
  in (a2,str1 ++ str2, i2)
printPrg' i Sync = ((),"Sync;\n",i)


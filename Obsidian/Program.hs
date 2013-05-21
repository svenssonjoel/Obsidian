{- Joel Svensson 2012,2013

   Notes:
   2013-04-02: Added a Break statement to the language.
               Use it to break out of sequential loops.
   2013-01-08: removed number-of-blocks field from ForAllBlocks

-}

{-# LANGUAGE GADTs, TypeFamilies  #-}
             


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

-- A hierarchy! 
data Step a -- A step in the hierarchy
data Zero
  
type Thread = Zero 
type Block  = Step Thread 
type Grid   = Step Block  


type family Below a

type instance Below Zero = Zero
type instance Below (Step Zero) = Zero
type instance Below (Step (Step Zero)) = Step Zero 

type Identifier = Int 
      
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
  SeqFor :: EWord32 -> (EWord32 -> Program t ())
            -> Program t ()
            
  SeqWhile :: Exp Bool ->
              Program Thread () ->
              Program Thread () 
  

            
  Break  :: Program Thread () 
 
  ForAll :: EWord32 
            -> (EWord32 -> Program t ())
            -> Program (Step t) ()

  --ForAllBlocks :: EWord32 -> (EWord32 -> Program Block ()) 
  --                -> Program Grid ()

  ForAllThreads :: (EWord32) -> (EWord32 -> Program Thread ())
                   -> Program Grid ()

  -- Allocate shared memory in each MP

  -- TODO: Change the Liveness analysis to a two-pass algo
  --       and remove the Allocate constructor. 
  Allocate :: Name -> Word32 -> Type -> Program Block () 

  -- Automatic Variables
  Declare :: Name -> Type -> Program t () 
              
  {- About Output (Creates a named output array). 
     This is similar to Allocate but concerning global arrays.

     Since we cannot synchronize writes to a global array inside of an
     kernel, global arrays will only be written as outputs of the kernel

     Also used this when doing 
  -}
  
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
  --Par :: Program p () ->
  --       Program p () ->
  --       Program p () 

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
-- forAll 
---------------------------------------------------------------------------
forAll :: EWord32 -> (EWord32 -> Program t ()) -> Program (Step t) ()
forAll n f = ForAll n f

---------------------------------------------------------------------------
-- SeqFor
---------------------------------------------------------------------------
seqFor :: EWord32 -> (EWord32 -> Program Thread ())
            -> Program Thread ()
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
forAllT :: EWord32 -> (EWord32 -> Program Thread ())
           -> Program Grid ()
forAllT n f = ForAllThreads n 
            $ \gtid -> f gtid 



forAllBlocks = forAll

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
-- runPrg (RETHINK!) (Works for Block programs, but all?)
---------------------------------------------------------------------------
runPrg :: Int -> Program t a -> (a,Int)
runPrg i Identifier = (i,i+1)

-- Maybe these two are the most interesting cases!
-- Return may for example give an array. 
runPrg i (Return a) = (a,i)
runPrg i (Bind m f) =
  let (a,i') = runPrg i m
  in runPrg i' (f a)
     
runPrg i (Sync) = ((),i)
runPrg i (ForAll n ixf) =
  let (p,i') = runPrg i (ixf (variable "tid")) 
  in  (p,i')
-- What can this boolean depend upon ? its quite general!
--  (we know p returns a ()... ) 
runPrg i (Cond b p) = ((),i) 
runPrg i (Declare _ _) = ((),i)
runPrg i (Allocate _ _ _ ) = ((),i)
runPrg i (Assign _ _ a) = ((),i) -- Probaby wrong.. 
runPrg i (AtomicOp _ _ _) = (variable ("new"++show i),i+1)

{- What do I want from runPrg ?

   # I want to it to "work" for all block programs (no exceptions)
   # I want a BProgram (Pull a) to return a Pull array of "correct length)
-}

                            
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
--printPrg' i (ForAllBlocks n f) =
--  let (d,prg2,i') = printPrg' i (f (variable "BIX"))
--  in (d, 
--      "blocks (i)" ++
--      "{\n" ++ prg2 ++ "\n}",
--      i')
printPrg' i (Return a) = (a,"MonadReturn;\n",i)
printPrg' i (Bind m f) =
  let (a1, str1,i1) = printPrg' i m
      (a2,str2,i2) = printPrg' i1 (f a1)
  in (a2,str1 ++ str2, i2)
printPrg' i Sync = ((),"Sync;\n",i)


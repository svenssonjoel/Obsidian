{- Joel Svensson 2012,2013

   Notes:
   2013-04-02: Added a Break statement to the language.
               Use it to break out of sequential loops.
   2013-01-08: removed number-of-blocks field from ForAllBlocks

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}




module Obsidian.Program  (
  -- Hierarchy 
  Thread, Block, Grid, Step, Zero, Warp, 
  -- Program type
  -- CoreProgram(..),
  Program(..), -- all exported.. for now
  TProgram, BProgram, GProgram, WProgram(..), 

  -- Class
  Sync, 
  
  -- helpers
  printPrg,
  runPrg,
  uniqueNamed, uniqueNamed_,  

  assign, allocate, declare,
  atomicOp, 
  -- Programming interface
  seqFor, forAll, {-forAll2,-} seqWhile, sync, distrPar,
  ) where 
 
import Data.Word
import Data.Monoid

import Obsidian.Exp
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Atomic
import Obsidian.Names

import Obsidian.Dimension

-- Package value-supply
import Data.Supply
import System.IO.Unsafe

import Control.Monad
import Control.Applicative


---------------------------------------------------------------------------
-- Thread/Block/Grid 
---------------------------------------------------------------------------



-- A hierarchy! 
data Step a -- A step in the hierarchy
data Zero 
  
type Thread = Zero
type Warp   = Step Thread
type Block  = Step Warp
type Grid   = Step Block





---------------------------------------------------------------------------

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
           
  -- 4 March 2014, Changed so that AtOp does not return a result. 
  -- Change this back later if an application requires. 
  AtomicOp :: Scalar a
              => Name        -- Array name 
              -> Exp Word32  -- Index to operate on 
              -> Atomic a    -- Atomic operation to perform 
              -> Program Thread ()

  Cond :: Exp Bool
          -> Program Thread ()
          -> Program Thread ()
                
  SeqWhile :: Exp Bool ->
              Program Thread () ->
              Program Thread () 
              
  Break  :: Program Thread () 


  -- use threads along one level
  -- Warp, Block, Grid. 
  ForAll :: Shape d => Dims d EWord32         -- Not sure EWord32 is really what I want
            -> (Index d -> Program Thread ())
            -> Program t () 

  
  -- Distribute over Warps yielding a Block
  -- Distribute over Blocks yielding a Grid 
  DistrPar :: EWord32
           -> (EWord32 -> Program t ())
           -> Program (Step t) ()
           -- (really Step t -> Step (Step t) ) 
                           
  SeqFor :: EWord32 -> (EWord32 -> Program t ())
            -> Program t ()

                           
 
  --        #w          warpId     
  --NWarps :: EWord32 -> (EWord32 -> Program Warp ()) -> Program Block () 

  --WarpForAll :: EWord32 
  --              -> (EWord32 -> Program Thread ()) 
  --              -> Program Warp ()
  -- WarpAllocate :: Name -> Word32 -> Type -> Program Warp ()  -- For now. 

  -- Allocate shared memory in each MP
  Allocate :: Name -> Word32 -> Type -> Program t () 

  -- Automatic Variables
  Declare :: Name -> Type -> Program t () 
                
  Sync     :: Program Block ()

  -- Parallel composition of Programs
  -- TODO: Will I use this ? 
  --Par :: Program p () ->
  --       Program p () ->
  --       Program p () 

  -- Monad
  Return :: a -> Program t a
  Bind   :: Program t a -> (a -> Program t b) -> Program t b

---------------------------------------------------------------------------
-- Aliases 
---------------------------------------------------------------------------
type TProgram = Program Thread
type WProgram = Program Warp 
type BProgram = Program Block
type GProgram = Program Grid 

-- -- Programs are a reader monad 
-- newtype Program t a = Program (EWord32 -> CoreProgram t a)

-- instance Monad (Program t )where
--     return x = Program $ \ _ -> return x
--     -- :: WProgram a -> (a -> WProgram b) -> WProgram b
--     (Program h) >>= f = Program
--                          $ \w ->
--                          do
--                            a <- h w
--                            let (Program g) = f a
--                            g w


-- core :: Program t a -> EWord32 ->  CoreProgram t a
-- core (Program f) id = f id 
---------------------------------------------------------------------------
-- Helpers 
--------------------------------------------------------------------------- 
uniqueSM = do
  id <- Identifier
  return $ "arr" ++ show id 

uniqueNamed pre = do
  id <- Identifier
  return $ pre ++ show id 

uniqueNamed_ pre = do id <- Identifier
                      return $ pre ++ show id 
---------------------------------------------------------------------------
-- Memory 
---------------------------------------------------------------------------
assign :: Scalar a => Name -> [Exp Word32] -> (Exp a) -> Program Thread ()
assign nom ix e = Assign nom ix e 

allocate :: Name -> Word32 -> Type -> Program t () 
allocate nom l t = Allocate nom l t 

declare :: Name -> Type -> Program t ()
declare nom t = Declare nom t 

---------------------------------------------------------------------------
-- atomicOp 
---------------------------------------------------------------------------
atomicOp :: Scalar a
            => Name        -- Array name 
            -> Exp Word32  -- Index to operate on 
            -> Atomic a    -- Atomic operation to perform 
            -> Program Thread ()
atomicOp nom ix atop = AtomicOp nom ix atop 

---------------------------------------------------------------------------
-- forAll 
---------------------------------------------------------------------------
forAll :: Shape d => Dims d EW32 -> (Index d -> Program Thread ()) -> Program t ()
forAll n f = ForAll n $ \ix -> f ix
  
-- forAll :: EWord32 -> (EWord32 -> Program t ()) -> Program (Step t) ()
-- forAll n f = Program $ \id -> ForAll n $ \ix -> core (f ix) id

--forAll2 :: EWord32
--           -> EWord32
--           -> (EWord32 -> EWord32 -> Program Thread ())
--           -> Program (Step (Step t)) ()
--forAll2 b n f =  DistrPar b $ \bs -> ForAll n $ \ix -> f bs ix 

-- forAll2 :: EWord32
--            -> EWord32
--            -> (EWord32 -> EWord32 -> Program t ())
--            -> Program (Step (Step t)) ()
-- forAll2 b n f =  forAll b $ \bs -> forAll n (f bs)

distrPar :: EWord32
           -> (EWord32 -> Program t ())
           -> Program (Step t) ()
distrPar b f = DistrPar b $ \bs -> f bs

---------------------------------------------------------------------------
-- warpForAll 
---------------------------------------------------------------------------
--warpForAll :: EWord32 -> (EWord32 -> Program Thread ()) -> Program Warp ()
-- warpForAll n f = Program $ \id -> WarpForAll n $ \ix -> core (f ix) id 
 
---------------------------------------------------------------------------
-- seqFor
---------------------------------------------------------------------------
seqFor :: EWord32 -> (EWord32 -> Program t ()) -> Program t ()
seqFor (Literal 1) f = f 0
seqFor n f = SeqFor n $ \ix -> f ix

---------------------------------------------------------------------------
-- seqWhile
---------------------------------------------------------------------------
seqWhile :: Exp Bool -> Program Thread () -> Program Thread ()
seqWhile b prg = SeqWhile b prg 

---------------------------------------------------------------------------
-- Monad
--------------------------------------------------------------------------
instance Monad (Program t) where
  return = Return
  (>>=) = Bind

---------------------------------------------------------------------------
-- Functor
---------------------------------------------------------------------------
instance Functor (Program t) where
  fmap g fa = do {a <- fa; return $ g a}

---------------------------------------------------------------------------
-- Applicative 
---------------------------------------------------------------------------
instance Applicative (Program t) where
  pure = return
  ff <*> fa = 
    do
      f <- ff
      fmap f fa

---------------------------------------------------------------------------
-- Class Sync
---------------------------------------------------------------------------
class Sync t where
  sync :: Program t () 

instance Sync Warp where
  sync = return ()

instance Sync Thread where
  sync = return ()

instance Sync Block where
  sync = Sync

instance Sync Grid where
  sync = error "sync: not implemented on grid computations" 
  -- (implement this using counters and locks)

---------------------------------------------------------------------------
-- runPrg (RETHINK!) (Works for Block programs, but all?)
---------------------------------------------------------------------------
-- I often need to run programs enough to figure out what size array they
-- holding. Really think through what is needed and what is not.
-- For example, this is only interesting when sizes are static.
-- Are there special conditions that hold in that case?
--  * Programs cannot compute lengths of static sized arrays for example.
--  * An array must be a result of a Return node.
--  * The Length of that array must be simple Arithmetic on known quantities.
--  A special version of run (Program t array -> Word32) should be possible. 
  
runPrg :: Int -> Program t a -> (a,Int)
runPrg i Identifier = (i,i+1)

-- Maybe these two are the most interesting cases!
-- Return may for example give an array. 
runPrg i (Return a) = (a,i)
runPrg i (Bind m f) =
  let (a,i') = runPrg i m
  in runPrg i' (f a)
     
runPrg i (Sync) = ((),i)
--runPrg i (ForAll n ixf) =
--  let (p,i') = runPrg i (ixf (variable "tid")) 
--  in  (p,i')
runPrg i (DistrPar n f) =
  let (p,i') = runPrg i (f (variable "DUMMY"))
  in (p,i')
-- What can this boolean depend upon ? its quite general!
--  (we know p returns a ()... ) 
runPrg i (Cond b p) = ((),i) 
runPrg i (Declare _ _) = ((),i)
runPrg i (Allocate _ _ _ ) = ((),i)
runPrg i (Assign _ _ a) = ((),i) -- Probaby wrong.. 
runPrg i (AtomicOp _ _ _) = ((),i) -- variable ("new"++show i),i+1)

{- What do I want from runPrg ?

   # I want to it to "work" for all block programs (no exceptions)
   # I want a BProgram (Pull a) to return a Pull array of "correct length)
-}

                            
---------------------------------------------------------------------------
-- printPrg (REIMPLEMENT) xs
---------------------------------------------------------------------------
printPrg :: Program t a -> String
printPrg prg = (\(_,x,_) -> x) $ printPrg' 0 prg

printPrg' :: Int -> Program t a -> (a,String,Int)
printPrg' i Identifier = (i,"getId;\n",i+1) 
printPrg' i (Assign n ix e) =
  ((),n ++ "[" ++ show ix ++ "] = " ++ show e ++ ";\n", i) 
printPrg' i (AtomicOp n ix e) =
  let newname = "r" ++ show i
  --in (variable newname,
  --    newname ++ " = " ++ printAtomic e ++
  --    "( " ++ n ++ "[" ++ show ix ++ "])\n",i+1)
  in ((), printAtomic e ++
          "( " ++ n ++ "[" ++ show ix ++ "])\n",i+1)
printPrg' i (Allocate id n t) =
  let newname = id -- "arr" ++ show id
  in ((),newname ++ " = malloc(" ++ show n ++ ");\n",i+1)
printPrg' i (Declare id t) =
  let newname = id -- "arr" ++ show id
  in ((),show t ++ " " ++ newname ++ "\n",i+1)
printPrg' i (SeqFor n f) =
  let (a,prg2,i') = printPrg' i (f (variable "i"))
      
  in ( a,  
       "for (i in 0.." ++ show n ++ ")" ++
       "{\n" ++ prg2 ++ "\n}",
       i')
     
--printPrg' i (ForAll n f) =
--  let (a,prg2,i') = printPrg' i (f (variable "i"))
      
--  in ( a,  
--       "par (i in 0.." ++ show n ++ ")" ++
--       "{\n" ++ prg2 ++ "\n}",
--       i')
printPrg' i (Return a) = (a,"MonadReturn;\n",i)
printPrg' i (Bind m f) =
  let (a1, str1,i1) = printPrg' i m
      (a2,str2,i2) = printPrg' i1 (f a1)
  in (a2,str1 ++ str2, i2)
printPrg' i Sync = ((),"Sync;\n",i)


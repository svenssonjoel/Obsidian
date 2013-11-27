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
             


module Obsidian.Program  (
  -- Hierarchy 
  Thread, Block, Grid, Warp, 
  -- Program type 
  Program(..), -- all exported.. for now
  TProgram, BProgram, GProgram, WProgram(..), Prog(..), p_app, ProgToProgram(..),

  -- Class
  Sync, 
  
  -- helpers
  printPrg,
  runPrg,
  uniqueNamed,

  -- Programming interface
  seqFor, forAll, forAll2, warpForAll,  seqWhile, sync,
  forAllG, nWarps, 
  -- module Control.Applicative                          
  ) where 
 
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

import Control.Monad
import Control.Applicative 

---------------------------------------------------------------------------
-- Thread/Block/Grid 
---------------------------------------------------------------------------

-- A hierarchy! 
--data Step a -- A step in the hierarchy
--data Zero
  
--type Thread = Zero 
--type Block  = Step Thread 
--type Grid   = Step Block

-- The Hierarchy is a bit risky. 
data Thread = Thread 
data Warp   = Warp -- outside the hierarchy 
data Block  = Block
data Grid   = Grid 


type Identifier = Int
                  
---------------------------------------------------------------------------
-- Program datatype
---------------------------------------------------------------------------
data Program t a where

  -------------------------------------------------------------------------
  -- Thread Programs 
  -------------------------------------------------------------------------
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
          -> Program Thread ()
          -> Program Thread ()
  
  SeqFor :: EWord32 -> (EWord32 -> Program t ())
            -> Program t ()
            
  SeqWhile :: Exp Bool ->
              Program Thread () ->
              Program Thread () 
              
  Break  :: Program Thread ()

  
  -------------------------------------------------------------------------
  -- Block Programs 
  -------------------------------------------------------------------------
  Sync     :: Program Block ()
            
  ForAll :: EWord32 
            -> (EWord32 -> Program Thread ())
            -> Program Block ()

  --        #w          warpId     
  NWarps :: EWord32 -> (EWord32 -> Program Warp ()) -> Program Block () 

  -------------------------------------------------------------------------
  -- Warp Programs 
  -------------------------------------------------------------------------
  WarpForAll :: EWord32 
                -> (EWord32 -> Program Thread ()) 
                -> Program Warp ()

  Allocate :: Name -> Word32 -> Type -> Program t () 

  -------------------------------------------------------------------------
  -- Grid Programs 
  -------------------------------------------------------------------------
  GForAll :: EWord32
             -> (EWord32 -> Program Block ())
             -> Program Grid ()
              

  -------------------------------------------------------------------------
  -- Generic Programs 
  -------------------------------------------------------------------------
  -- Automatic Variables
  Declare :: Name -> Type -> Program t () 

  -- Monad
  Return :: a -> Program t a
  Bind   :: Program t a -> (a -> Program t b) -> Program t b

    
  Identifier :: Program t Identifier 

    
  
--data Device a = Device

--data Grid a where
  -- = Concat :: (EWord32 -> Program Block (SPush Block a)) -> Grid (Device a)
--  Concat :: (Pull a -> Pull (Program Block (SPush Block b))) -> Device a -> Grid (Device b) 
  
  


---------------------------------------------------------------------------
-- Aliases 
---------------------------------------------------------------------------
--type TProgram = Program Thread
--type BProgram = Program Block
--type GProgram = Program Grid 

type TProgram = Prog Thread
type BProgram = Prog Block
type GProgram = Prog Grid 
type WProgram = Prog Warp 

-- WPrograms are a reader monad
--newtype WProgram a = WProgram (EWord32 -> Program Warp a)

--instance Monad WProgram where
--    return x = WProgram $ \ _ -> return x
    -- :: WProgram a -> (a -> WProgram b) -> WProgram b
--    (WProgram h) >>= f = WProgram
--                         $ \w ->
--                         do
--                           a <- h w
--                           let (WProgram g) = f a
--                           g w

-- Reader, knows "identity"
--  identity is a dummy for all programs but Warp programs
--  at the moment
data Prog t a = Prog (EWord32 -> Program t a)

instance Monad (Prog t) where
  return x = Prog $ \ _ -> return x
  (Prog h) >>= f = Prog
                   $ \id ->
                   do
                     a <- h id
                     let (Prog g) = f a
                     g id

p_app (Prog f) x = f x 

applyIn f id ix = f ix `p_app` id

-- Much hacking, little thought. 
class ProgToProgram t where
  progToProgram :: Prog t a -> Program t a

instance ProgToProgram Warp where
  progToProgram (Prog f) = f (variable "warpID")

instance ProgToProgram Block where
  progToProgram (Prog f) = f (variable "DUMMY_BLOCK_ID")

instance ProgToProgram Thread where
  progToProgram (Prog f) = f (variable "DUMMY_THREAD_ID")

instance ProgToProgram Grid where
  progToProgram (Prog f) = f (variable "DUMMY_GRID_ID")


---------------------------------------------------------------------------
-- Helpers 
--------------------------------------------------------------------------- 
uniqueSM = Prog $ \_ -> do
  id <- Identifier
  return $ "arr" ++ show id 

uniqueNamed pre = Prog $ \_ -> do
  id <- Identifier
  return $ pre ++ show id 

---------------------------------------------------------------------------
-- forAll 
---------------------------------------------------------------------------
forAll :: EWord32 -> (EWord32 -> Prog Thread ()) -> Prog Block ()
forAll n f = Prog $ \id -> ForAll n (applyIn f id)
 
forAll2 :: EWord32
         -> EWord32
         -> (EWord32 -> EWord32 -> Prog Thread ())
         -> Prog Grid ()
forAll2 b n f = Prog $ \id -> GForAll b $ applyIn (\bs -> forAll n (f bs)) id

forAllG n f = Prog $ \id -> GForAll n (applyIn f id) 


warpForAll n f = Prog $ \id -> WarpForAll n (applyIn f id)

nWarps n f = Prog $ \id -> NWarps n (applyIn f (variable "warpID"))

-- WarpForAll (sizeConv n) $ \i -> wf (ixf i) i

---------------------------------------------------------------------------
-- seqFor
---------------------------------------------------------------------------
-- if dummy here, fix for Warp
seqFor :: EWord32 -> (EWord32 -> Prog t ()) -> Prog t ()
seqFor (Literal 1) f = f 0
seqFor n f = Prog $ \id -> SeqFor n (applyIn f id)

---------------------------------------------------------------------------
-- seqWhile
---------------------------------------------------------------------------
seqWhile :: Exp Bool -> Program Thread () -> Program Thread ()
seqWhile = SeqWhile 

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
class Monad p => Sync p where
  sync :: p () 

instance Sync (Prog Warp) where
  sync = return ()

instance Sync (Prog Thread) where
  sync = return ()

instance Sync (Prog Block) where
  sync = Prog $ \_ -> Sync

--instance Sync (Program Grid) where
--  sync = error "sync: not implemented" 
--  -- (implement this using counters and locks)

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


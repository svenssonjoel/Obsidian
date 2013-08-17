{-# LANGUAGE GADTs,
             ExistentialQuantification,
             FlexibleInstances #-}

{- CodeGen.Program.

   Joel Svensson 2012, 2013

   Notes:
     2013-03-17: Codegeneration is changing
-} 


module Obsidian.CodeGen.Program where

import Obsidian.Exp
import Obsidian.Globs
import Obsidian.Types
import Obsidian.Atomic

import qualified Obsidian.Program as P
import Obsidian.Program (Step,Zero)

import Data.Word
import Data.Supply
import Data.List

import System.IO.Unsafe



---------------------------------------------------------------------------
-- New Intermediate representation
---------------------------------------------------------------------------

type IMList a = [(Statement a,a)]

type IM = IMList ()

-- out :: 
out a = [(a,())]

data Statement t = SAssign Name [IExp] IExp
         --         | SAtomicOp Name Name (IExp) (Atomic a)
                 | SCond IExp (IMList t) 
                 | SSeqFor String IExp (IMList t)
                 | SBreak
                 | SSeqWhile IExp (IMList t)
                   
                 | SForAll IExp (IMList t) 
                 | SForAllBlocks IExp (IMList t)

    -- Memory Allocation..
                 | SAllocate Name Word32 Type
                 | SDeclare  Name Type
                 | SOutput   Name Type

    -- Synchronisation
                 | SSynchronize

compileStep1 :: Compile t => P.Program t a -> IM
compileStep1 p = snd $ compile ns p
  where
    ns = unsafePerformIO$ newEnumSupply


class Compile t where
  compile :: Supply Int -> P.Program t a -> (a,IM)

-- Compile Thread program 
instance Compile Zero  where 
  compile s p = cs s p 

-- Compile Block program 
instance Compile (Step Zero) where
  compile s (P.ForAll n f) = (a,out (SForAll (expToIExp n) im))
    where
      p = f (ThreadIdx X)
      (a,im) = compile s p
  compile s p = cs s p 

-- Compile a Grid Program 
instance Compile (Step (Step (Zero))) where
  compile s (P.ForAll n f) = (a, out (SForAllBlocks (expToIExp n) im))
    where 
      p = f (BlockIdx X)
      (a,im) = compile s p
  compile s p = cs s p


---------------------------------------------------------------------------
-- General compilation
---------------------------------------------------------------------------
cs :: Compile t => Supply Int -> P.Program t a -> (a,IM) 
cs i P.Identifier = (supplyValue i, [])

cs i (P.Assign name ix e) =
  ((),out (SAssign name (map expToIExp ix) (expToIExp e)))
 
--cs i (P.AtomicOp name ix at) = (v,out im)
--  where 
--    nom = "a" ++ show (supplyValue i)
--    v = variable nom
--    im = SAtomicOp nom name ix at
      
cs i (P.Cond bexp p) = ((),out (SCond (expToIExp bexp) im)) 
  where ((),im) = compile i p

cs i (P.SeqFor n f) = (a,out (SSeqFor nom (expToIExp n) im))
  where
    (i1,i2) = split2 i
    nom = "i" ++ show (supplyValue i1)
    v = variable nom
    p = f v
    (a,im) = compile i2 p
cs i (P.SeqWhile b p) = (a, out (SSeqWhile (expToIExp b) im))
  where
    (a,im) = compile i p

cs i (P.Break) = ((), out SBreak)

cs i (P.Allocate id n t) = ((),out (SAllocate id n t))
cs i (P.Declare  id t)   = ((),out (SDeclare id t))
-- Output works in a different way! (FIX THIS!)
--  Uniformity! (Allocate Declare Output) 
cs i (P.Output   t)      = (nom,out (SOutput nom t))
  where nom = "output" ++ show (supplyValue i) 
cs i (P.Sync)            = ((),out (SSynchronize))


cs i (P.Bind p f) = (b,im1 ++ im2) 
  where
    (s1,s2) = split2 i
    (a,im1) = compile s1 p
    (b,im2) = compile s2 (f a)

cs i (P.Return a) = (a,[])

-- The nested ForAll case. 
cs i p = error $ P.printPrg p -- compile i p 



---------------------------------------------------------------------------
-- Analysis
--------------------------------------------------------------------------- 
numThreads :: IMList a -> Maybe Word32 
numThreads im = foldl maxCheck (Just 0) $ map process im
  where
    process (SCond bexp im,_) = numThreads im
    process (SSeqFor _ _ _,_) = Just 1
    process (SForAll (IWord32 n) _,_) = Just n
    process (SForAll n _,_) = Nothing
    process (SForAllBlocks _ im,_) = numThreads im
--     process (SForAllThreads n im,_) = Right (ariable "UNKNOWN") --fix this!
    process a = Just 0 -- ok ? 

    maxCheck (Just a) (Just  b)  = Just  $ max a b
    maxCheck _ _ = Nothing



getOutputs :: IMList a -> [(Name,Type)]
getOutputs im = concatMap process im
  where
    process (SOutput name t,_)      = [(name,t)]
    process (SSeqFor _ _ im,_)      = getOutputs im
    process (SForAll _ im,_)        = getOutputs im
    process (SForAllBlocks _ im,_)  = getOutputs im
--    process (SForAllThreads _ im,_) = getOutputs im
    process a = []
    

---------------------------------------------------------------------------
-- Turning IM to strings
---------------------------------------------------------------------------

printIM :: Show a => IMList a -> String 
printIM im = concatMap printStm im
  
-- Print a Statement with metadata 
printStm :: Show a => (Statement a,a) -> String
printStm (SAssign name [] e,m) =
  name ++ " = " ++ show e ++ ";" ++ meta m
printStm (SAssign name ix e,m) =
  name ++ "[" ++ concat (intersperse "," (map show ix)) ++ "]" ++
  " = " ++ show e ++ ";" ++ meta m
--printStm (SAtomicOp res arr ix op,m) =
--  res ++ " = " ++
--  printAtomic op ++ "(" ++ arr ++ "[" ++ show ix ++ "]);" ++ meta m
printStm (SAllocate name n t,m) =
  name ++ " = malloc(" ++ show n ++ ");" ++ meta m
printStm (SDeclare name t,m) =
  show t ++ " " ++ name ++ ";" ++ meta m
printStm (SOutput name t,m) =
  show t ++ " " ++ name ++ ";" ++ meta m
printStm (SCond bexp im,m) =
  "if " ++ show bexp ++ "{\n" ++ 
  concatMap printStm im ++ "\n};" ++ meta m

printStm (SSynchronize,m) =
  "sync();" ++ meta m
  
printStm (SSeqFor name n im,m) =
  "for " ++ name  ++ " in [0.." ++ show n ++"] do" ++ meta m ++ 
  concatMap printStm im ++ "\ndone;\n"

printStm (SForAll n im,m) =
  "forAll i in [0.." ++ show n ++"] do" ++ meta m ++
  concatMap printStm im ++ "\ndone;\n"

printStm (SForAllBlocks n im,m) =
  "forAllBlocks i in [0.." ++ show n ++"] do" ++ meta m ++
  concatMap printStm im ++ "\ndone;\n"
--printStm (SForAllThreads n im,m) =
--  "forAllThreads i in [0.." ++ show n ++"] do" ++ meta m ++ 
--  concatMap printStm im ++ "\ndone;\n"


  
-- printStm (a,m) = error $ show m 

meta :: Show a => a -> String
meta m = "\t//" ++ show m ++ "\n" 

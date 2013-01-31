{-# LANGUAGE GADTs,
             ExistentialQuantification #-}

{- CodeGen.Program.

   To get somewhere at pace I will transform the new Program datatype
   (monad) into a backend-program type that is more or less identical
   to the old one (the one that all the code generation expects)

   Joel Svensson 2012
-} 


module Obsidian.CodeGen.Program where

import Obsidian.Exp
import Obsidian.Globs
import Obsidian.Types
import Obsidian.Atomic

import qualified Obsidian.Program as P 

import Data.Word
import Data.Supply

import System.IO.Unsafe
---------------------------------------------------------------------------
-- Program
--------------------------------------------------------------------------- 
data Program extra
     = Skip
     | forall a. Scalar a =>
       Assign Name (Exp Word32) (Exp a) 
     | forall a. Scalar a =>
       AtomicOp Name Name (Exp Word32) (Atomic a)

     | Cond (Exp Bool) (Program extra)
       
     | SeqFor String (Exp Word32) (Exp Word32 -> Program extra) 
     | ForAll (Maybe Word32) (Exp Word32 -> Program extra)
       -- Not present in old Program datatype
     | ForAllBlocks (Exp Word32 -> Program extra)
     | Allocate Name Word32 Type extra
       -- Not Present in old Program datatype 
     | Output Name Type 
     | Synchronize Bool
       
     | ProgramPar (Program extra)
                  (Program extra)
       
     | ProgramSeq (Program extra)
                  (Program extra)

---------------------------------------------------------------------------
-- Program translation from (P.Program a) to (Program ()) 
---------------------------------------------------------------------------

evalPrg p = runPrg' ns p
  where ns = unsafePerformIO$ newEnumSupply 

convPrg = runPrg 

runPrg p = snd$ runPrg' ns p 
  where ns = unsafePerformIO$ newEnumSupply

runPrg' :: Supply Int -> P.Program t a -> (a,Program ())
runPrg' i P.Identifier = (supplyValue i,Skip) 
runPrg' i (P.Assign name ix e) = ((),Assign name ix e)
runPrg' i (P.AtomicOp name ix at) =
  let nom = "a" ++ show (supplyValue i)
  in  (variable nom,AtomicOp nom name ix at)
runPrg' i (P.Cond bexp p) =
  let ((),p') = runPrg' i p
  in ((),Cond bexp p') 
runPrg' i (P.SeqFor n f) =
  let newf = (\x -> snd (runPrg' i (f x)))
      nom = "i" ++ show (supplyValue i) 
  in ((),SeqFor nom n newf) 
runPrg' i (P.ForAll n f) =
  let newf = (\x -> snd (runPrg' i (f x)))
  in  ((),ForAll n newf)
runPrg' i (P.ForAllBlocks f) =
  let newf = (\x -> snd (runPrg' i (f x)))
  in ((),ForAllBlocks newf)
--runPrg' i (P.ForAllG f) =
--  let newf = (\x -> snd (runPrg' i (f x)))
--  in ((),ForAllG newf)
runPrg' i (P.Par p1 p2) = ((),ProgramPar p1' p2')
  where
    (s1,s2) = split2 i 
    ((),p1') = runPrg' s1 p1
    ((),p2') = runPrg' s2 p2
    
runPrg' i (P.Bind p f) =
  let (s1,s2) = split2 i
      (a,prg1) = runPrg' s1 p
      (b,prg2) = runPrg' s2 (f a)
  in (b,prg1 `ProgramSeq` prg2)
runPrg' i (P.Return a) = (a,Skip)
runPrg' i (P.Allocate id n t) = ((), Allocate id n t ()) 
  -- let nom = "arr" ++ show id -- (supplyValue i)
  
runPrg' i (P.Output t) =
  let nom = "output" ++ show (supplyValue i)
  in  (nom, Output nom t)
runPrg' i (P.Sync)     = ((),Synchronize True)

---------------------------------------------------------------------------
-- Printing Programs
---------------------------------------------------------------------------

printPrg :: Show e => Program e -> String 
printPrg Skip = ";\n"
printPrg (Assign name ix e) =
  name ++ "["++ printExp ix ++ "]" ++
  " = " ++ printExp e ++ ";\n"
printPrg (AtomicOp res arr ix op) =
  res ++ " = " ++
  printAtomic op ++ "(" ++ arr ++ "[" ++ printExp ix ++ "]);\n"
printPrg (Cond bexp p) =
  "if " ++ printExp bexp ++ " { \n" ++
  printPrg p ++ "\n"
  ++ "}"
printPrg (SeqFor nom n f) =
  "seqFor "++ nom ++ " in [0.."++show n++"] do\n" ++
  printPrg (f (variable nom)) ++ "\ndone;\n"
printPrg (ForAll n f) =
  "forAll i in [0.."++show n ++"] do\n" ++
  printPrg (f (variable "i")) ++ "\ndone;\n"
printPrg (ForAllBlocks f) =
  "Blocks i do\n" ++
  printPrg (f (variable "i")) ++ "\ndone;\n"
--printPrg (ForAllG f) =
--  "Blocks i do\n" ++
--  printPrg (f (variable "i")) ++ "\ndone;\n"
printPrg (Allocate nom n t e) =
  nom ++ " = malloc(" ++ show n ++ ");\n" ++
  "***" ++ show e ++ "***\n"
printPrg (Output nom t) =
  nom ++ " = newOutput();\n"
printPrg (Synchronize b) = "sync();\n"

printPrg (ProgramPar p1 p2) =
  printPrg p1 ++ printPrg p2

printPrg (ProgramSeq p1 p2) =
  printPrg p1 ++ printPrg p2
  

---------------------------------------------------------------------------
-- Analyzing Programs
---------------------------------------------------------------------------

-- The function that gets num threads from programs  is
-- starting to look a bit fuzzy
threadsPerBlock :: Program e -> Word32
threadsPerBlock (Cond e p)     = threadsPerBlock p
threadsPerBlock (SeqFor _ _ _) = 1
threadsPerBlock (ForAll (Just n) f) = n
threadsPerBlock (ForAll Nothing f) = 0 -- really the answer is "Any number of threads" 
threadsPerBlock (ForAllBlocks f) = threadsPerBlock (f (variable "X"))
threadsPerBlock (p1 `ProgramSeq` p2) =
  max (threadsPerBlock p1)
      (threadsPerBlock p2)
threadsPerBlock (p1 `ProgramPar` p2) =
  max (threadsPerBlock p1)
      (threadsPerBlock p2)
threadsPerBlock _ = 0 -- Should be ok

collectOutputs :: Program e -> [(Name,Type)]
collectOutputs (Cond e p) = collectOutputs p 
collectOutputs (Output nom t) = [(nom,t)]
collectOutputs (p1 `ProgramSeq` p2) = collectOutputs p1 ++
                                      collectOutputs p2
collectOutputs (p1 `ProgramPar` p2) = collectOutputs p1 ++
                                      collectOutputs p2
collectOutputs (SeqFor _ _ f) = collectOutputs (f (variable "X"))
-- collectOutputs (ForAll n f) = collectOutputs (f (variable "X"))
collectOutputs (ForAllBlocks f) = collectOutputs (f (variable "X"))
--collectOutputs (ForAllG f) = collectOutputs (f (variable "X"))
collectOutputs a = []

---------------------------------------------------------------------------
-- Extract a "kernel" from a program 
---------------------------------------------------------------------------
{-
   INFO: First do this in the most simple way.
         Allow only one-kernel-programs. That is a single ForAllBlocks,
         no sequences of them allowed.
   TODO: In future enable turning a program into multiple kernels (perhaps). 
-} 

extract :: Program e -> Program e
extract prg = case extract' prg of
  Just p ->  p
  Nothing -> error "extract: unsupported program structure"
  
  where
    extract' (ForAllBlocks f) = Just (f (BlockIdx X))
    extract' (p1 `ProgramSeq` p2) =
      case (extract' p1, extract' p2) of
        (Nothing, Nothing) -> Nothing
        (Nothing, p) -> p 
        (p, Nothing) -> p
        (p1,p2) -> error "extract: two programs in sequence"
    extract' (p1 `ProgramPar` p2) =
      case (extract' p1, extract' p2) of
        (Nothing, Nothing) -> Nothing
        (Nothing, p) -> p 
        (p, Nothing) -> p
          -- There is more than meets the eye going on here.
          -- The resulting ProgramSeq is at a different "level"
          -- compared to the original ProgramPar
        (Just p1,Just p2) -> Just $ p1 `ProgramSeq` p2 
    extract' p = Nothing


-- Assumes that program does not do anything
-- strange with regards to global arrays. 
flatten :: Program e -> Program e
flatten (ForAllBlocks f) = f (BlockIdx X)
flatten (p1 `ProgramSeq` p2) = flatten p1 `ProgramSeq` flatten p2
flatten (p1 `ProgramPar` p2) = flatten p1 `ProgramSeq` flatten p2 -- ALERT! 
flatten p = p  
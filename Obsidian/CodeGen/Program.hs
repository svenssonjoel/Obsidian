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
     | ForAll Word32 (Exp Word32 -> Program extra)
       -- Not present in old Program datatype
     | ForAllBlocks {-(Exp Word32)-} (Exp Word32 -> Program extra)
     | Allocate Name Word32 Type extra
       -- Not Present in old Program datatype 
     | Output Name Type 
     | Synchronize Bool
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
runPrg' i (P.Assign name ix e) = ((),Assign name ix e)
runPrg' i (P.AtomicOp name ix at) =
  let nom = "a" ++ show (supplyValue i)
  in  (variable nom,AtomicOp nom name ix at) 
runPrg' i (P.ForAll n f) =
  let newf = (\x -> snd (runPrg' i (f x)))
  in  ((),ForAll n newf)
runPrg' i (P.ForAllBlocks f) =
  let newf = (\x -> snd (runPrg' i (f x)))
  in ((),ForAllBlocks newf)
runPrg' i (P.Bind p f) =
  let (s1,s2) = split2 i
      (a,prg1) = runPrg' s1 p
      (b,prg2) = runPrg' s2 (f a)
  in (b,prg1 `ProgramSeq` prg2)
runPrg' i (P.Return a) = (a,Skip)
runPrg' i (P.Allocate n t) =
  let nom = "arr" ++ show (supplyValue i)
  in (nom, Allocate nom n t ()) 
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
printPrg (ForAll n f) =
  "forAll i in [0.."++show n ++"] do\n" ++
  printPrg (f (variable "i")) ++ "\ndone;\n"
printPrg (ForAllBlocks f) =
  "Blocks i do\n" ++
  printPrg (f (variable "i")) ++ "\ndone;\n"
printPrg (Allocate nom n t e) =
  nom ++ " = malloc(" ++ show n ++ ");\n" ++
  "***" ++ show e ++ "***\n"
printPrg (Output nom t) =
  nom ++ " = newOutput();\n"
printPrg (Synchronize b) = "sync();\n"
printPrg (ProgramSeq p1 p2) =
  printPrg p1 ++ printPrg p2
  

---------------------------------------------------------------------------
-- Analyzing Programs
---------------------------------------------------------------------------

threadsPerBlock :: Program e -> Word32
threadsPerBlock (ForAll n f) = n
threadsPerBlock (ForAllBlocks f) = threadsPerBlock (f (variable "X"))
threadsPerBlock (p1 `ProgramSeq` p2) =
  max (threadsPerBlock p1)
      (threadsPerBlock p2)
threadsPerBlock _ = 0 -- Should be ok

collectOutputs :: Program e -> [(Name,Type)]
collectOutputs (Output nom t) = [(nom,t)]
collectOutputs (p1 `ProgramSeq` p2) = collectOutputs p1 ++
                                      collectOutputs p2
collectOutputs (ForAll n f) = collectOutputs (f (variable "X"))
collectOutputs (ForAllBlocks f) = collectOutputs (f (variable "X"))
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
    extract' p = Nothing
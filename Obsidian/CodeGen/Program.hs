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
import Data.List

import System.IO.Unsafe
---------------------------------------------------------------------------
-- Program
--------------------------------------------------------------------------- 
data Program extra
     = Skip
     | forall a. Scalar a =>
       Assign Name [Exp Word32] (Exp a) 
     | forall a. Scalar a =>
       AtomicOp Name Name (Exp Word32) (Atomic a)

     | Cond (Exp Bool) (Program extra)
       
     | SeqFor String (Exp Word32) (Exp Word32 -> Program extra) 
     | ForAll (Exp Word32) (Exp Word32 -> Program extra)
       -- Not present in old Program datatype
     | ForAllBlocks (Exp Word32) (Exp Word32 -> Program extra)
     | ForAllThreads (Exp Word32) (Exp Word32 -> Program extra)  
       
     | Allocate Name Word32 Type extra
     | Declare  Name Type 
       -- Not Present in old Program datatype 
     | Output Name Type 
     | Synchronize Bool
       
     | ProgramPar (Program extra)
                  (Program extra)
       
     | ProgramSeq (Program extra)
                  (Program extra)


---------------------------------------------------------------------------
-- PrgTfrm
---------------------------------------------------------------------------

prgTfrm (p1 `ProgramSeq` p2) = prgTfrm p1 `appendLast` prgTfrm p2 
prgTfrm p = p

appendLast (p1 `ProgramSeq` p2) p3 = p1 `ProgramSeq` appendLast p2 p3
appendLast p1 p2 = p1 `ProgramSeq` p2

---------------------------------------------------------------------------
-- Program translation from (P.Program a) to (Program ()) 
---------------------------------------------------------------------------

evalPrg p = runPrg' ns p
  where ns = unsafePerformIO$ newEnumSupply 

convPrg = runPrg 

runPrg p = {- prgTfrm $-}  snd$ runPrg' ns p 
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
runPrg' i (P.ForAllBlocks n f) =
  let newf = (\x -> snd (runPrg' i (f x)))
  in ((),ForAllBlocks n newf)
runPrg' i (P.ForAllThreads n f) =
  let newf = (\x -> snd (runPrg' i (f x)))
  in ((),ForAllThreads n newf)
     
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
runPrg' i (P.Declare id t) = ((),Declare id t)
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
  name ++ "["++ concat (intersperse "," (map printExp ix)) ++ "]" ++
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
printPrg (ForAllBlocks n f) =
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
threadsPerBlock (ForAll (Literal n) f) = n
threadsPerBlock (ForAll _ f) = error "Not a literal" 
-- threadsPerBlock (ForAll  f) = 0 -- really the answer is "Any number of threads" 
threadsPerBlock (ForAllBlocks _ f) = threadsPerBlock (f (variable "X"))
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
collectOutputs (ForAllBlocks m f) = collectOutputs (f (variable "X"))
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
    extract' (ForAllBlocks m f) = Just (f (BlockIdx X))
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
flatten (ForAllBlocks m f) = f (BlockIdx X)
flatten (p1 `ProgramSeq` p2) = flatten p1 `ProgramSeq` flatten p2
flatten (p1 `ProgramPar` p2) = flatten p1 `ProgramSeq` flatten p2 -- ALERT! 
flatten p = p  



---------------------------------------------------------------------------
-- New Intermediate representation
---------------------------------------------------------------------------

type IMList a = [(Statement a,a)]

type IM = IMList ()

-- out :: 
out a = [(a,())]


data Statement t = forall a. (Show a, Scalar a) => SAssign Name [Exp Word32] (Exp a)
               | forall a. (Show a, Scalar a) => SAtomicOp Name Name (Exp Word32) (Atomic a)
               | SCond (Exp Bool) (IMList t) 
               | SSeqFor String (Exp Word32) (IMList t)
                 -- See if it is possible to get away
                 -- with only one kind of ForAll (plus maybe some flag) 
               | SForAll (Exp Word32) (IMList t) 
               | SForAllBlocks (Exp Word32) (IMList t)
                 -- a special loop over all threads..
               | SForAllThreads (Exp Word32) (IMList t)

                 -- Memory Allocation..
               | SAllocate Name Word32 Type
               | SDeclare  Name Type
               | SOutput   Name Type

                 -- Synchronisation
               | SSynchronize

                 -- ProgramPar and ProgramSeq does not exist
                -- at this level (the par or seq info is lost!)
               

compileStep1 :: P.Program t a -> IM
compileStep1 p = snd $ cs1 ns p
  where
    ns = unsafePerformIO$ newEnumSupply

cs1 :: Supply Int -> P.Program t a -> (a,IM) 
cs1 i P.Identifier = (supplyValue i, [])

cs1 i (P.Assign name ix e) = ((),out (SAssign name ix e))

cs1 i (P.AtomicOp name ix at) = (v,out im)
  where 
    nom = "a" ++ show (supplyValue i)
    v = variable nom
    im = SAtomicOp nom name ix at
      
cs1 i (P.Cond bexp p) = ((),out (SCond bexp im)) 
  where ((),im) = cs1 i p


cs1 i (P.SeqFor n f) = (a,out (SSeqFor nom n im))
  where
    (i1,i2) = split2 i
    nom = "i" ++ show (supplyValue i1)
    v = variable nom
    p = f v
    (a,im) = cs1 i2 p 
    

cs1 i (P.ForAll n f) = (a,out (SForAll n im))
  where
    p = f (ThreadIdx X)  
    (a,im) = cs1 i p 


cs1 i (P.ForAllBlocks n f) = (a,out (SForAllBlocks n im)) 
  where
    p = f (BlockIdx X)
    (a,im) = cs1 i p


-- Warning: Every thread will ALWAYS need to perform a conditional
--     (Only in special case is the conditional not needed) 
-- TRY To express all library functions using ForAllBlocks + ForAll
-- For more flexibility and probably in the end performance. 
cs1 i (P.ForAllThreads n f) = (a,out (SForAllThreads n im)) 
  where
    p = f (BlockIdx X * BlockDim X + ThreadIdx X)
    (a,im) = cs1 i p


cs1 i (P.Allocate id n t) = ((),out (SAllocate id n t))
cs1 i (P.Declare  id t)   = ((),out (SDeclare id t))
-- Output works in a different way! (FIX THIS!) 
cs1 i (P.Output   t)      = (nom,out (SOutput nom t))
  where nom = "output" ++ show (supplyValue i) 
cs1 i (P.Sync)            = ((),out (SSynchronize))


cs1 i (P.Bind p f) = (b,im1 ++ im2) 
  where
    (s1,s2) = split2 i
    (a,im1) = cs1 s1 p
    (b,im2) = cs1 s2 (f a)

cs1 i (P.Return a) = (a,[])


---------------------------------------------------------------------------
--
---------------------------------------------------------------------------

-- Print a Statement with metadata 
printStm :: Show a => (Statement a,a) -> String
printStm (SAssign name [] e,m) =
  name ++ " = " ++ printExp e ++ ";" ++ meta m
printStm (SAssign name ix e,m) =
  name ++ "[" ++ concat (intersperse "," (map printExp ix)) ++ "]" ++
  " = " ++ printExp e ++ ";" ++ meta m
printStm (SAtomicOp res arr ix op,m) =
  res ++ " = " ++
  printAtomic op ++ "(" ++ arr ++ "[" ++ printExp ix ++ "]);" ++ meta m
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
  "for " ++ name  ++ " in [0.." ++ show n ++"] do \n" ++
  concatMap printStm im ++ "\ndone;" ++ meta m

printStm (SForAll n im,m) =
  "forAll i in [0.." ++ show n ++"] do \n" ++
  concatMap printStm im ++ "\ndone;" ++ meta m

printStm (SForAllBlocks n im,m) =
  "forAllBlocks i in [0.." ++ show n ++"] do \n" ++
  concatMap printStm im ++ "\ndone;" ++ meta m
printStm (SForAllThreads n im,m) =
  "forAllThreads i in [0.." ++ show n ++"] do \n" ++
  concatMap printStm im ++ "\ndone;" ++ meta m


  
-- printStm (a,m) = error $ show m 

meta :: Show a => a -> String
meta m = "\t//" ++ show m ++ "\n" 
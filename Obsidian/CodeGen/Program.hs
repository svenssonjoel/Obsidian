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
import Obsidian.Program  -- (Step,Zero)

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

data Statement t = SAssign IExp [IExp] IExp
         --         | SAtomicOp Name Name (IExp) (Atomic a)
                 | SCond IExp (IMList t) 
                 | SSeqFor String IExp (IMList t)
                 | SBreak
                 | SSeqWhile IExp (IMList t)

                 --        Iters   Body
                 | SForAll IExp    (IMList t) 
                 | SForAllBlocks IExp (IMList t)
 
                 | SNWarps IExp (IMList t)
                 --                  (ThreadIM)
                 | SWarpForAll  IExp (IMList t)                    
--                 | SWarpForAll String   String IExp (IMList t) 

    -- Memory Allocation..
                 | SAllocate Name Word32 Type
                 | SDeclare  Name Type

    -- Synchronisation
                 | SSynchronize

usesWarps :: IMList t -> Bool
usesWarps = any (go . fst)
  where
--    go (SWarpForAll _ _ _ _) = True
    go (SWarpForAll _ _) = True
    go (SNWarps _ _) = True
    go (SForAllBlocks _ im) = usesWarps im 
    go _ = False

usesTid :: IMList t -> Bool
usesTid = any (go . fst)
  where
  --  go (SForAll _ _ _) = True
    go (SForAll _ _) = True
    go (SForAllBlocks _ im) = usesTid im
    go _ = False 


---------------------------------------------------------------------------
-- COmpilation of Program to IM
--------------------------------------------------------------------------- 

compileStep1 :: (ProgToProgram t, Compile t) => P.Prog t a -> IM
compileStep1 p = snd $ compile ns (progToProgram p)
  where
    ns = unsafePerformIO$ newEnumSupply


class Compile t where
  compile :: Supply Int -> P.Program t a -> (a,IM)

-- Compile Thread program 
instance Compile Thread where 
  compile s p = cs s p 

-- Compile Block program 
instance Compile Block where
  compile s (P.ForAll n f) = (a,out (SForAll (expToIExp n) im))
    where
      --(i1,i2) = split2 s
      nom = "tid" 
      v = variable nom
      p = f v  -- (ThreadIdx X)
      (a,im) = compile s p
  compile s (P.NWarps n f) = ((),im) -- out (SNWarps (expToIExp n) im))
      where
        ((),im) = compileW s n (f (variable "warpID"))
  compile s p = cs s p


-- Compile a Warp program
compileW :: Supply Int -> EWord32 -> P.Program P.Warp a -> (a,IM)
compileW i nWarps@(Literal nw) prg = go $ prg --(variable warpIDNom) -- (tid `div` 32)
  where
    warpIDNom = "warpID"
    warpIxNom = "warpIx"
    go :: P.Program P.Warp a -> (a,IM)
    go (P.WarpForAll iters prgf)
      = (a,out $ SNWarps (expToIExp nWarps) [(SWarpForAll (expToIExp iters) im,())])
      where
        p = prgf (variable warpIxNom) -- correct
        (a,im) = compile i p    -- Compile the inner threadProgram
    go (P.Allocate nom n t)
      = ((),out (SAllocate nom (nw*n) t))
    go (P.Bind p f) = (b,(im1 ++ im2))
      where
        (s1,s2) = split2 i
        (a,im1) = compileW s1 nWarps p 
        (b,im2) = compileW s2 nWarps (f a)
    go (P.Return a) = (a,[])
    go (P.Identifier) = (supplyValue i, [])


-- Compile a Grid Program 
instance Compile Grid where
  compile s (P.GForAll n f) = (a, out (SForAllBlocks (expToIExp n) im))
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
  ((),out (SAssign (IVar name (typeOf e)) (map expToIExp ix) (expToIExp e)))
 
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
-- Turning IM to strings
---------------------------------------------------------------------------

printIM :: Show a => IMList a -> String 
printIM im = concatMap printStm im
  
-- Print a Statement with metadata 
printStm :: Show a => (Statement a,a) -> String
printStm (SAssign name [] e,m) =
  show name ++ " = " ++ show e ++ ";" ++ meta m
printStm (SAssign name ix e,m) =
  show name ++ "[" ++ concat (intersperse "," (map show ix)) ++ "]" ++
  " = " ++ show e ++ ";" ++ meta m
--printStm (SAtomicOp res arr ix op,m) =
--  res ++ " = " ++
--  printAtomic op ++ "(" ++ arr ++ "[" ++ show ix ++ "]);" ++ meta m
printStm (SAllocate name n t,m) =
  name ++ " = malloc(" ++ show n ++ ");" ++ meta m
printStm (SDeclare name t,m) =
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
  "forAll tid" ++ "  in [0.." ++ show n ++"] do" ++ meta m ++
  concatMap printStm im ++ "\ndone;\n"

printStm (SForAllBlocks n im,m) =
  "forAllBlocks i in [0.." ++ show n ++"] do" ++ meta m ++
  concatMap printStm im ++ "\ndone;\n"

printStm (SNWarps e im,m) = "NWarps " ++ show e ++ " { \n" ++
  concatMap printStm im ++ "}\n"
printStm (SWarpForAll n im,m) =
  "warpForAll tid" ++ "  in [0.." ++ show n ++"] do" ++ meta m ++
  concatMap printStm im ++ "\ndone;\n" 
--printStm (SForAllThreads n im,m) =
--  "forAllThreads i in [0.." ++ show n ++"] do" ++ meta m ++ 
--  concatMap printStm im ++ "\ndone;\n"

--printStm (x,m) = error $ show x 

  
-- printStm (a,m) = error $ show m 

meta :: Show a => a -> String
meta m = "\t//" ++ show m ++ "\n" 



      


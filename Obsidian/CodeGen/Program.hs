{-# LANGUAGE GADTs,
             ExistentialQuantification,
             FlexibleInstances  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}

{- CodeGen.Program.

   Joel Svensson 2012..2015

   Notes:
     2013-03-17: Codegeneration is changing
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

import Control.Monad.State
import Control.Applicative 


---------------------------------------------------------------------------
-- New Intermediate representation
---------------------------------------------------------------------------

type IMList a = [(Statement a,a)]

type IM = IMList ()

out :: a -> [(a,())] 
out a = [(a,())]

-- Atomic operations
data AtOp = AtInc
          | AtAdd  IExp 
          | AtSub  IExp 
          | AtExch IExp 


data HLevel = Thread
            | Warp
            | Block
            | Grid
            

-- Statements 
data Statement t = SAssign IExp [IExp] IExp
                 | SAtomicOp IExp IExp AtOp
                 | SCond IExp (IMList t) 
                 | SSeqFor String IExp (IMList t)
                 | SBreak
                 | SSeqWhile IExp (IMList t)

                 --               Iters   Body
                 | SForAll HLevel IExp    (IMList t)
                 | SDistrPar HLevel IExp  (IMList t)
                   
    -- Memory Allocation..
                 | SAllocate Name Word32 Type
                 | SDeclare  Name Type

    -- Synchronisation
                 | SSynchronize


--------------------------------------------------------------------------- 
-- Collect and pass around data during first step compilation
data Context = Context { ctxNWarps :: Maybe Word32,
                         ctxNThreads :: Maybe Word32, 
                         ctxGLBUsesTid :: Bool,
                         ctxGLBUsesWid :: Bool}

newtype CM a = CM (State Context a)
   deriving (Monad, MonadState Context, Functor, Applicative)

runCM :: CM a -> Context -> a 
--runCM (CM cm) ctx = evalState cm ctx
runCM (CM cm) = evalState  cm 

evalCM :: CM a -> Context -> (a, Context)
evalCM (CM cm) = runState cm 

setUsesTid :: CM ()
setUsesTid = modify $ \ctx -> ctx { ctxGLBUsesTid = True } 

setUsesWid :: CM ()
setUsesWid = modify $ \ctx -> ctx { ctxGLBUsesWid = True } 

enterWarp :: Word32 -> CM ()
enterWarp  n = modify $ \ctx -> ctx { ctxNWarps = Just n }

enterThread :: Word32 -> CM ()
enterThread n = modify $ \ctx -> ctx {ctxNThreads = Just n} 

clearWarp :: CM ()
clearWarp = modify $ \ctx -> ctx {ctxNWarps = Nothing}

getNWarps :: CM (Maybe Word32)
getNWarps = do
  ctx <- get
  return $ ctxNWarps ctx

getNThreads :: CM (Maybe Word32)
getNThreads = do
  ctx <- get
  return $ ctxNThreads ctx 

emptyCtx :: Context
emptyCtx = Context Nothing Nothing False False
---------------------------------------------------------------------------


-- Sort these out and improve! 
usesWarps :: IMList t -> Bool
usesWarps = any (go . fst)
  where
    go (SDistrPar _ _ im) = usesWarps im 
    go (SForAll Warp _ _) = True
    go _ = False

usesTid :: IMList t -> Bool
usesTid = any (go . fst)
  where
    go (SDistrPar _ _ im) = usesTid im 
    go (SForAll Block _ _) = True
    go (SSeqFor _ _ im) = usesTid im 
    go _ = False
usesBid :: IMList t -> Bool
usesBid = any (go . fst)
  where
    go (SDistrPar Block _ _) = True -- usesBid im
    -- go (SForAll Block _ _) = True
    go _ = False
usesGid :: IMList t -> Bool
usesGid = any (go . fst)
  where
    go (SForAll Grid _ _) = True
    go _ = False
 


---------------------------------------------------------------------------
-- COmpilation of Program to IM
--------------------------------------------------------------------------- 

compileStep1 :: Compile t => P.Program t a -> IM
compileStep1 p = snd $ runCM (compile ns p) emptyCtx
  where
    ns = unsafePerformIO$ newEnumSupply


class Compile t where
  compile :: Supply Int -> P.Program t a -> CM (a,IM)

-- Compile Thread program 
instance Compile P.Thread  where
  -- Can add cases for P.ForAll here.
  -- Turn into sequential loop. Could be important to make push
  -- operate uniformly across entire hierarchy.
  compile s (P.ForAll n f) = 
    do
      let (i1,i2) = split2 s
          nom = "i" ++ show (supplyValue i1)
          v = variable nom
          p = f v
      (a,im) <-  compile i2 p

      return ((),out $ SSeqFor nom (expToIExp n) im)
  -- AllocateSM can nolonger occur on this level 
  -- compile s (P.AllocateSM nom n t) = do
  --   (Just nt) <- getNThreads -- must be a Just at this point
  --   nw' <- getNWarps

  --   let nw = case nw' of
  --         Nothing -> 1
  --         Just i  -> i
    
  --   return ((),out $ SAllocate nom (nt*nw*n) t)
  compile _ (P.Sync) =
    return ((),[])

  compile s p = cs s p 

-- Compile Warp program
instance Compile P.Warp where
  compile s (P.DistrPar n f) =
    error "Currently not supported to distribute over the threads, use ForAll instead!"
  compile s (P.ForAll n@(Literal n') f) = do

    -- setup context to know number of threads
    -- executing 
    enterThread n'
    
    let p = f (variable "warpIx") 
    (a,im) <- compile s p 
    return (a, out $ SForAll Warp (expToIExp n) im)
    --undefined -- compile a warp program that iterates over a space n large

  -- AllocateSM can nolonger occur on this level 
  -- compile s (P.AllocateSM nom n t) = do
  --   (Just nw) <- getNWarps -- Must be a Just here, or something is wrong!
  --   return ((),out $ SAllocate nom (nw*n) t)
  -- compile s (P.Bind p f) = do
  --   let (s1,s2) = split2 s
  --   (a,im1) <- compile s1 p
  --   (b,im2) <- compile s2 (f a)
  --   return (b,(im1 ++ im2))
  compile s (P.Return a) = return (a,[])
  compile s (P.Identifier) = return (supplyValue s, [])
  compile s (P.Sync) = return ((),[])
  -- Why no fallthrough here ?
  -- Adding (must have been a horrible mistake!)
  compile s p = cs s p
  
-- Compile Block program 
instance Compile P.Block where
  compile s (P.ForAll n@(Literal n') f) = do

    -- Set up the context to know the number of
    -- concurrent thread programs that are executing. 
    enterThread n'
    setUsesTid
    
    let nom = "tid"
        v   = variable nom
        p   = f v
    
    (a,im) <-  compile s p
    -- in this case a could be () (since it is guaranteed to be anyway). a
    return (a,out (SForAll Block (expToIExp n) im))
    
  compile s (P.DistrPar n'@(Literal n) f) = do
    
    {- Distribute work over warps! -}
    -- Set up the context for the compilation
    -- of the Warp code.
    -- BUG: Something like this is needed for distribution
    -- over threads too!
    -- FIXED: Bug mentioned above should be (at least) partially fixed. 
    enterWarp n
    -- Number of active warps are stored in the context. 
    (a,im) <- compile s (f (variable "warpID"))
    return (a, out (SDistrPar Warp (expToIExp n') im))
  compile s (P.AllocateSM id n t) = return ((),out (SAllocate id n t))
  compile s (P.Sync) = return ((),out (SSynchronize))
  compile s p = cs s p

-- Compile a Grid Program 
instance Compile P.Grid where
  {- Distribute over blocks -}
  compile s (P.DistrPar n f) = do
    -- Need to generate IM here that the backend can read desired number of blocks from
    let p = f (variable "bid") -- "blockIdx.x") -- (BlockIdx X) 
    
    (a, im) <- compile s p -- (f (BlockIdx X)) 
    return (a, out (SDistrPar Block (expToIExp n) im))
  -- compile s (P.AllocateSM _ _ _) = error "Allocate at level Grid" 
  compile s p = cs s p

  {- ForAll cannot happen here! -}



---------------------------------------------------------------------------
-- General compilation
---------------------------------------------------------------------------
cs :: forall t a . Compile t => Supply Int -> P.Program t a -> CM (a,IM) 
cs i P.Identifier = return $ (supplyValue i, [])
cs i (P.Assign name ix e) =
  return $ ((),out (SAssign (IVar name (typeOf e)) (map expToIExp ix) (expToIExp e)))

cs i (P.AtomicOp name ix atom) =
  case atom of
    AtomicInc -> return $ ((),out (SAtomicOp (IVar name Word32) (expToIExp ix) AtInc))
    AtomicAdd e -> error $ "CodeGen.Program: AtomicAdd is not implemented"
    AtomicSub e -> error $ "CodeGen.Program: AtomicSub is not implemented" 
    AtomicExch e -> error $ "CodeGen.Program: AtomicExch is not implemented" 
      
cs i (P.Cond bexp p) = do
  ((),im) <-  compile i p
  return ((),out (SCond (expToIExp bexp) im)) 
 
cs i (P.SeqFor n f) = do
  let (i1,i2) = split2 i
      nom = "i" ++ show (supplyValue i1)
      v = variable nom
      p = f v
  (a,im) <-  compile i2 p
  
  return (a,out (SSeqFor nom (expToIExp n) im))

    
cs i (P.SeqWhile b p) = do
  (a,im) <-  compile i p
  return (a, out (SSeqWhile (expToIExp b) im))

    

cs i (P.Break) = return ((), out SBreak)

cs i (P.AllocateSM id n t) = error $ "CodeGen.Program: Allocate without a hierarchy designation" 

cs i (P.Declare  id t)   = return ((),out (SDeclare id t))

cs i (P.Bind p f) = do 
  let (s1,s2) = split2 i
  (a,im1) <- compile s1 p
  (b,im2) <- compile s2 (f a)

  return (b,im1 ++ im2) 
 
 
cs i (P.Return a) = return (a,[])


-- Unhandled cases 
cs i p = error $ "CodeGen.Program: unhandled in cs: " ++ P.printPrg p -- compile i p 

---------------------------------------------------------------------------
-- Turning IM to strings (outdated and broken) 
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


printStm (SForAll Warp n im,m) =
  "forAll wid" ++ "  in [0.." ++ show n ++"] do" ++ meta m ++
  concatMap printStm im ++ "\ndone;\n"

printStm (SForAll Block n im,m) =
  "forAll tid" ++ "  in [0.." ++ show n ++"] do" ++ meta m ++
  concatMap printStm im ++ "\ndone;\n"

printStm (SForAll Grid n im,m) =
  "forAll gid in [0.." ++ show n ++"] do" ++ meta m ++
  concatMap printStm im ++ "\ndone;\n"

printStm (SDistrPar lvl n im,m) = 
  "forAll gid in [0.." ++ show n ++"] do" ++ meta m ++
  concatMap printStm im ++ "\ndone;\n"

meta :: Show a => a -> String
meta m = "\t//" ++ show m ++ "\n" 



      


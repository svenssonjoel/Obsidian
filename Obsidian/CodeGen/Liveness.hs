

{- Joel Svensson 2012, 2013

   notes:
     added case for SeqFor  Jan-21-2013

-} 
module Obsidian.CodeGen.Liveness where

import qualified Data.Set as Set

import Obsidian.Exp
import Obsidian.Globs
import Obsidian.CodeGen.Program

import Control.Monad.State
------------------------------------------------------------------------------
-- LIVENESS on Programs

type Liveness = Set.Set Name

liveness :: Show e => Program e -> Program Liveness
liveness p = fst$ liveness' p Set.empty 
             
liveness' :: Show e => Program e -> Liveness -> (Program Liveness,Liveness) 
liveness' (Assign name ix exp ) s = (Assign name ix exp, living)
  where 
    arrays = collectArrays exp 
    living = Set.fromList arrays `Set.union` s
     -- Should not name be in the alive list ? 
    
liveness' (Allocate name size t _) s = 
  (Allocate name size t alive,alive)
  where 
    alive = name `Set.delete` s
            
-- Added Mar 2013
liveness' (Declare name t ) s = 
  (Declare name t,s)

-- Added Jan 2013
liveness' (AtomicOp n1 n2 ix op) s = (AtomicOp n1 n2 ix op, s)
 -- Is this correct. It would help if I remembered exactly what
 -- this does.

-- Added Jan 2013
liveness' (Cond bexp p) s = (Cond bexp p',living)
  where
    (p',aliveInside) = liveness' p Set.empty
    living = s `Set.union` aliveInside
             
-- Added Jan 2013 
liveness' (SeqFor nom n ixfToPrg) s = (SeqFor nom n (fst . ixf'),living)
  where
    ixf' = ((flip liveness') Set.empty) . ixfToPrg
    aliveInside = snd$ ixf' (variable "X")
    living = s `Set.union` aliveInside

liveness' (ForAllThreads n ixfToPrg) s = (ForAllThreads n (fst . ixf'),living)    
  where 
    ixf' = ((flip liveness') Set.empty) . ixfToPrg
    aliveInside = snd$ ixf' (variable "X") 
    living = s `Set.union` aliveInside


liveness' (ForAllBlocks n ixfToPrg) s = (ForAllBlocks n (fst . ixf'),living)    
  where 
    ixf' = ((flip liveness') Set.empty) . ixfToPrg
    aliveInside = snd$ ixf' (variable "X") 
    living = s `Set.union` aliveInside

liveness' (ForAll n ixfToPrg) s = (ForAll n (fst . ixf'),living)    
  where 
    ixf' = ((flip liveness') Set.empty) . ixfToPrg
    aliveInside = snd$ ixf' (variable "X") 
    living = s `Set.union` aliveInside
    -- NOTE: Performs local liveness check (to what use ?) 
--liveness' e@(Cond b p) s = (Cond b p',s') 
--  where 
--    (p',s') = liveness' p s 
  -- TODO: Change this if the conditions depend on 
  -- previously computed arrays 
  -- NOTE: Need to traverse p here just to shift its type to Program Liveness
             
liveness' (Synchronize b) s = (Synchronize b,s) 
liveness' Skip s = (Skip, s)

liveness' (p1 `ProgramSeq` p2) s = 
  (p1' `ProgramSeq` p2',l1) 
  where 
    (p2',l2) = liveness' p2 s
    (p1',l1) = liveness' p1 l2

--liveness' (p1 `ProgramSeq` p2) s = 
--  (p1' `ProgramSeq` p2',l1) 
--  where 
 --   (p2',l2) = liveness' p2 s
 --   (p1',l1) = liveness' p1 l2

liveness' (Output n t) s = (Output n t,s) -- error $ printPrg p 

 
                           
{- 
liveness :: Code a -> Code Liveness 
liveness (s `Seq` c) = lives `Seq` livec 
    where 
      lives = livenessSyncUnit aliveNext s  
      livec = liveness c
      aliveNext = whatsAliveNext livec

liveness Skip = Skip 
-} 
{- 
livenessSyncUnit aliveNext (SyncUnit nt prg _) = 
  SyncUnit nt prg alive
  where alive = livenessProgram aliveNext prg


livenessProgram aliveNext (Assign name ix e)  = livingArrs
  where 
    arrays = collectArrays e
    tmp    = Set.fromList arrays `Set.union` aliveNext
    livingArrs = name `Set.delete` tmp
livenessProgram aliveNext (ForAll f n) = livenessProgram aliveNext (f (variable "X"))    
livenessProgram aliveNext (Allocate name size t prg) = livenessProgram aliveNext prg
livenessProgram aliveNext (Cond c p) = livenessProgram aliveNext p
livenessProgram aliveNext (prg1 `ProgramSeq` prg2) = 
  livenessProgram aliveNext prg1 `Set.union` livenessProgram aliveNext prg2
-}

                           
-- TODO: Think about what kind of programs there 
--       will be. Is the below correct ? 
whatsAliveNext :: Program Liveness -> Liveness
whatsAliveNext Skip = Set.empty
whatsAliveNext (Synchronize _) = Set.empty
whatsAliveNext (Allocate _ _ _ l) = l
whatsAliveNext (Declare _ _) = Set.empty
whatsAliveNext (Assign _ _ _) = Set.empty
whatsAliveNext (ForAll _{-p-} _) = Set.empty 
-- I dont think any programs we generate will 
-- allocate things within a forAll! 
-- (Make this more clear in the type ?)
-- whatsAliveNext (Cond _ _ _ p) = whatsAliveNext p
-- Most likely Cond will not contain Alloc nodes
-- beneath it either.
                                   
whatsAliveNext (SeqFor _ _ _) = Set.empty
whatsAliveNext (ForAllBlocks n f) = s
  where
     s = whatsAliveNext (f (variable "X")) 
whatsAliveNext (p1 `ProgramSeq` p2) = 
  let s1 = whatsAliveNext p1  
      s2 = whatsAliveNext p2 
  in s1 `Set.union` s2
whatsAliveNext p = error $ printPrg p 
{-
whatsAliveNext :: Code Liveness -> Liveness
whatsAliveNext Skip = Set.empty
whatsAliveNext (s `Seq` _) = syncExtra s
-}
------------------------------------------------------------------------------ 

type IML = [(Statement Liveness,Liveness)] 


{- Plan:
   # Step through program from end to start
   # as soon as a new name is encountered, add it to the living set
   # when an "Allocate" is found, delete the name it allocated from the living set.

   Requirements:
   # All names are unique! 

   TODO: Think more carefully about the ForAllBlocks case
   TODO: Can ixs contain array names ?
           (Most likely yes! think about the counting sort example)

-} 
           

-- Nice type 
computeLiveness :: IMList a -> IML
computeLiveness im = reverse $ evalState (cl (reverse im)) Set.empty

-- Nice Type 
computeLiveness1 :: Liveness -> IMList a -> IML
computeLiveness1 l im = reverse $ evalState (cl (reverse im)) l

-- cl :: IM -> State Liveness IML 
cl im = mapM process im
  where
    safeHead [] = Set.empty
    safeHead (x:xs) = snd x

    -- Horrific type 
    process :: (Statement a,a) -> State Liveness (Statement Liveness,Liveness)
    process (SAssign nom ixs e,_) =
      do
        s <- get
        let arrays = collectArrays e
            living = Set.fromList (nom:arrays) `Set.union` s
        -- Need to recreate the statement (but at a different type, Haskell is "great") 
        return (SAssign nom ixs e,living)
    
    process (SAtomicOp n1 n2 ixs op,_) =
      do
        s <- get
        return (SAtomicOp n1 n2 ixs op,s)
        
    process (SAllocate name size t,_) =
      do
        modify (name `Set.delete`) 
        s <- get
        return (SAllocate name size t,s)  
    
    process (SDeclare name t,_) = 
      do 
        s <- get 
        return (SDeclare name t,s)

    process (SOutput name t,_) = 
      do 
        s <- get 
        return (SOutput name t,s)

    process (SSynchronize,_) = 
      do 
        s <- get
        return (SSynchronize,s) 

    process (SCond bexp im,_) = 
      do
        -- TODO: What should really happen here ?
        s <- get 
        let iml = computeLiveness1 s im 
            l   = safeHead iml 
            ns  =  s `Set.union` l
        put ns 
        -- Is this correct ?  Same question, all below
        return (SCond bexp iml,ns)

    process (SSeqFor nom n im,_) = 
      do 
        s <- get
        let iml = computeLiveness1 s im 
            l   = safeHead iml
        return (SSeqFor nom n iml, s `Set.union` l) 


    process (SForAll n im,_) =  
      do 
        s <- get 
        let iml = computeLiveness1 s im 
            l   = safeHead iml 
            ns  = s `Set.union` l
        put ns
        return (SForAll n iml,ns) 
    
    process (SForAllBlocks n im,_) = 
      do 
        s <- get 
        let iml = computeLiveness1 s im 
            l   = safeHead iml 
            ns  = s `Set.union` l
        put ns
        return (SForAllBlocks n iml,ns)

    process (SForAllThreads n im,_) = 
      do 
        s <- get 
        let iml = computeLiveness1 s im 
            l   = safeHead iml 
            ns  = s `Set.union` l
        put ns 
        return (SForAllThreads n iml,ns)


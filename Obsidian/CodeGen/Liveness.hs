

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


---------------------------------------------------------------------------
--
---------------------------------------------------------------------------
type Liveness = Set.Set Name

--------------------------------------------------------------------------- 
--
--------------------------------------------------------------------------- 
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
        let arrays = collectArraysI "arr" e
            arrays1 = collectArraysI "arr" nom
            living = Set.fromList (arrays1++arrays) `Set.union` s
        
        put living  -- update state   
        return (SAssign nom ixs e,living)
    process (SAtomicOp nom ix atop,_) =
      do
        s <- get
        let arrays =
              case atop of
                AtInc -> []
                AtAdd e -> collectArraysI "arr" e
                AtSub e -> collectArraysI "arr" e
                AtExch e -> collectArraysI "arr" e
            arrays1 = collectArraysI "arr" nom
            living = Set.fromList (arrays1++arrays) `Set.union` s
        put living
        return (SAtomicOp nom ix atop, living) 
    
--    process (SAtomicOp n1 n2 ixs op,_) =
--      do
--        s <- get
--        return (SAtomicOp n1 n2 ixs op,s)
        
    process (SAllocate name size t,_) =
      do
        modify (name `Set.delete`)
        s <- get        
        return (SAllocate name size t,s)  
    
    process (SDeclare name t,_) = 
      do 
        s <- get 
        return (SDeclare name t,s)

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
            ns  = s `Set.union` l
        put ns
        return (SSeqFor nom n iml,ns) 
    process (SSeqWhile b im,_) = 
      do 
        s <- get 
        let iml = computeLiveness1 s im 
            l   = safeHead iml 
            ns  = s `Set.union` l
        put ns 
        return (SSeqWhile b iml,ns)
    process (SBreak,_) = 
      do 
        s <- get 
        return (SBreak,s)


    process (SForAll lvl n im,_) =  
      do 
        s <- get 
        let iml = computeLiveness1 s im 
            l   = safeHead iml 
            ns  = s `Set.union` l
        put ns
        return (SForAll lvl n iml,ns) 
    process (SDistrPar lvl n im,_) = 
      do 
        s <- get 
        let iml = computeLiveness1 s im 
            l   = safeHead iml 
            ns  = s `Set.union` l 
        put ns 
        return (SDistrPar lvl n iml,ns) 
    -- process (SForAllBlocks n im,_) = 
    --   do 
    --     s <- get 
    --     let iml = computeLiveness1 s im 
    --         l   = safeHead iml 
    --         ns  = s `Set.union` l
    --     put ns
    --     return (SForAllBlocks n iml,ns)
    -- process (SNWarps n im,_) = 
    --   do 
    --     s <- get
    --     let iml = computeLiveness1 s im 
    --         l   = safeHead iml 
    --         ns  = s `Set.union` l
    --     put ns
    --     return (SNWarps n iml,ns)
    -- process (SWarpForAll n im,_) =  
    --   do 
    --     s <- get 
    --     let iml = computeLiveness1 s im 
    --         l   = safeHead iml 
    --         ns  = s `Set.union` l
    --     put ns
    --     return (SWarpForAll n iml,ns) 


    -- process (SForAllThreads n im,_) = 
    --   do 
    --     s <- get 
    --     let iml = computeLiveness1 s im 
    --         l   = safeHead iml 
    --         ns  = s `Set.union` l
    --     put ns 
    --     return (SForAllThreads n iml,ns)


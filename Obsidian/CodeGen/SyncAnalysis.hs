
{- Joel Svensson 2012 -} 
module Obsidian.CodeGen.SyncAnalysis where

-- The contents of this module is broken! 
{- 
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Globs

import Data.Word
import Data.List
import qualified Data.Map as Map

{- 
   SyncAnalysis. 

   Try to decide automatically whether a Sync is needed or not. 
   *** This will make sence in the CUDA case. For OpenCL generation 
       use the programmer-placed syncs. (This is why the Synchronize 
       constructor in the Program datatype now has a boolean parameter. 
   
   The method I use to decide if a Sync is needed or not is:
    When Array 'arr' is computed, if a thread, t1, that computes 
    an element uses any element owned by* a thread, t2, such 
    that (t1 `div` WarpSize /= t2 `div` WarpSize) that has not 
    yet been synced. 
      If so, a sync is needed before t1 performs the computation. 
    *Owned by means having been computed by. 

   
   To implement the above method I use two maps and a single pass 
     over the Program datastructure. 
   SAMap contains at anytime all the arrays that have not yet been synced    
      it maps  array-names to a TEMap 
   TEMap is a Index to ThreadId  that for every index remembers what 
      thread computed it. 

   So when computing location tid in an array, every array and index 
   that that location depends on is found. The array-names are used 
   to make lookups into SAMap followed by looking up the in the TEMap 
   using the index. If the thread obtained is in another warp a sync 
   is inserted and the SAMap cleared. 
   
   The array currently being computed is added to the SAMap and no 
   sync is inserted following its computation. 

  
   Future:
     * Once we start to write data-dependent assignment locations (like filters) 
       The implementation needs an update to work. 
       One way to take care of this might be to simply say that if the index 
       cannot be evaluated here it is data-dependent and a sync should be used. 
       (see eval and evalSource, local functions in analyseForAll) 
     
     * The implementation does not understand nested ForAll loops.
       If we ever use nested ForAlls this needs to change. 


     ERROR!!! 
       My assumptions about when it was safe not to sync are faulty! 
       This needs more thought!
      
-} 

-- TEMap is an Index->Thread map
-- TODO: Replace with an Array. If that is more efficient.
type TEMap = Map.Map Word32 Word32 


-- SAMap is a name->TEMap map 
type SAMap = Map.Map Name TEMap

-- TODO: Right now warps are 32 threads. 
--       This might change in the future. 
warpSize = 32

--syncAnalysis :: Program a -> Program a
--syncAnalysis prg = prg

{- 
  input program should have unique names for all intermediate arrays
-} 
syncAnalysis :: Program a -> Program a
syncAnalysis prg = prg 

{-
syncAnalysis :: Program a -> Program a
syncAnalysis prg = snd$ syncAnalysis' prg Map.empty
  where 
    syncAnalysis' :: Program a -> SAMap -> (SAMap,Program a) 
    syncAnalysis' Skip sam = (sam,Skip) 
    syncAnalysis' (Synchronize b) sam = (sam,Synchronize False) 
    syncAnalysis' f@(ForAll _ _) sam = analyseForAll f sam 
    syncAnalysis' a@(Allocate nom n t e) sam = (sam,a) 
    syncAnalysis' (ProgramSeq prg1 prg2) sam = 
      let (sam1,prg1') = syncAnalysis' prg1 sam 
          (sam2,prg2') = syncAnalysis' prg2 sam1
      in (sam2,prg1' `ProgramSeq` prg2')


-- The below case should just take place within a ForAll case.
    syncAnalysis' (Assign nom ix a) sam = error "should not happen" 
    


    analyseForAll (ForAll g n) sam = (sam'',if sNeeded
                                            then Synchronize True *>* ForAll g n 
                                            else ForAll g n ) 
                                 -- error$ show arrloc -- (sam',prg) 
      where                                   
        threads   = [0..(n-1)]
        gPrgs     = [(g (fromIntegral tid),tid) | tid <- threads] 
    
        arrloc''   = concatMap getSourceIndices gPrgs  
        arrloc'    = filter pred arrloc''
        arrloc     = map evalSource arrloc'
        targetMaps = map getTargIndex gPrgs -- (zip gPrgs threads)
    
        (sam',sNeeded) = conflict arrloc sam

        sam''          =  addMappings targetMaps sam'
         
    
        eval (Literal a) = a 
        evalSource (n,(a,ix)) = (n,(a,eval ix))
        pred (_,(x,_)) = not ("input" `isPrefixOf` x) 
    
-}
-- TODO: look at the special case below. (Make more beautiful) 
getSourceIndices :: (Program a,Word32) -> [(Word32,(Name,Exp Word32))] 
getSourceIndices (Assign nom (Literal ix) a,tid) = map (\y -> (tid,y)) (collectArrayIndexPairs a)
-- Special case! 
getSourceIndices (Assign nom ix _,tid) = 
  if isPrefixOf "result" nom || 
     isPrefixOf "input" nom then [] 
  else error$ "getSourceIndices: " ++ show ix ++ " is not Literal"
getSourceIndices _ = error "getSourceIndices: Can only handle a very simple case so far"

-- What array are we computing now, 
-- create the threadId -> Ix map for that array 
getTargIndex ((Assign nom (Literal ix) a),tid) = (nom,(ix,tid))--(nom,(ix,tid)) 

-- What characterizes a conflict  
conflict :: [(Word32,(Name,Word32))] -> SAMap -> (SAMap,Bool)
conflict [] sam = (sam,False) 
conflict ((thread,(arr,ix)):xs) sam = 
  case Map.lookup arr sam of 
    Nothing  -> error$ "this should not be possible" ++ "\n" ++ show ix ++ "\n" ++ show sam  -- conflict xs sam 
    (Just m) -> case Map.lookup ix m of  
                  Nothing -> error$ "this should not be possible" ++ "\n" ++ show ix ++ "\n" ++ show m 
                  (Just j) -> 
                              
                    if (thread `div` 32 /= j `div` 32)
                    then (Map.empty,True) -- We have a conflict
                    else conflict xs sam  
                         
                      
-- What thread is computing what "now".
-- Add that info to the SAMap
addMappings :: [(Name,(Word32,Word32))] -> SAMap -> SAMap 
addMappings [] sam = sam
addMappings ((nom,(ix,thread)):xs) sam = 
  let sam' = case Map.lookup nom sam of 
               Nothing -> Map.insert nom (Map.insert ix thread Map.empty) sam 
               (Just m) -> Map.insert nom (Map.insert ix thread m) sam 
  in addMappings xs sam'  


-} 

{- Joel Svensson 2012, 2013
 
   notes:
     Added a SeqFor case Jan-21-2013

 -} 
module Obsidian.CodeGen.Memory 
       (MemMap,
        Memory,
        allocate,
        free,
        freeAll,
        size, 
        sharedMem,  
        Address,
        Bytes,
        mmIM,
        renameIM ) 
       where 

import qualified Data.List as List
import qualified Data.Set  as Set
import Data.Word

import Obsidian.Types
import Obsidian.Globs

import Obsidian.Exp 
import Obsidian.CodeGen.Program
import Obsidian.CodeGen.Liveness

import qualified Data.Map as Map 

---------------------------------------------------------------------------
-- Memory layout
---------------------------------------------------------------------------

type MemMap = Map.Map Name (Word32,Type)

type Address = Word32
type Bytes   = Word32 

data Memory = Memory {freeList  :: [(Address,Bytes)] ,
                      allocated :: [(Address,Bytes)] , 
                      size      :: Bytes} -- how much used
            deriving Show 
              
              
-- 48 kilobytes of smem              
sharedMem = Memory [(0,49152)] [] 0


updateMax :: Memory -> Memory 
updateMax mem = let m = maximum [a+b|(a,b) <- allocated mem]
                    m' = max m (size mem)
                in mem {size = m'}

-- This one needs to check that shared memory is not full.
allocate :: Memory -> Bytes -> (Memory,Address)
allocate m b = 
  let adress = filter (\(x,y) -> y >= b) (freeList m) -- get a list of candidates
      getTop mem = let (a,b)  = case null (allocated m) of 
                         False -> maximum $ List.sort (allocated m) 
                         True  -> (0,0)
                   in a+b
  in case adress of 
    -- use the first candidate (try better approaches 
    -- such as searching for best match, so that not to waste memory)
    ((a,bytes):_)  -> let fl = filter (\(addr,_) -> a /= addr) (freeList m)
                          fl' = if b < bytes 
                                then (a+b,bytes-b):fl
                                else fl
                      in  (updateMax (m {freeList = fl', 
                                         allocated = (a,b):allocated m}) ,a)
    [] -> error "out of shared memory"
   
                          
free :: Memory -> Address -> Memory
free m a = mem 
    where 
      bytes = lookup a (allocated m)
      al    = filter (\(addr,_) -> a /= addr) (allocated m)

      -- TODO: Investigate this much closer.
      --       Is it a bug or is freeing a non allocated memory area
      --       OK?
      
      mem   = case bytes of 
                Nothing -> m
                {-
                  error $ "error: Address " ++ show a ++
                          " not found in allocated list" ++
                          "\n" ++ show m
                -} 
                Just b -> m {freeList = compress ((a,b):(freeList m)),
                             allocated = al}

freeAll :: Memory -> [Address] -> Memory 
freeAll m [] = m
freeAll m (a:as) = freeAll (free m a) as

compress = merge . List.sort 

merge [] = [] 
merge [x] = [x]
merge ((x,b):(y,b2):xs) = if (x+b == y)
                          then merge ((x,b+b2):xs) 
                          else (x,b):merge((y,b2):xs)

---------------------------------------------------------------------------
-- Memory map the new IM
---------------------------------------------------------------------------
mmIM :: IML -> Memory -> MemMap -> (Memory, MemMap)
mmIM im memory memmap = r im (memory,memmap)
  where
    r [] m = m
    r (x:xs) (m,mm) =
      let
          (m',mm') = process x m mm
           
          freeable = getFreeableSet x xs
          freeableAddrs = mapM (flip Map.lookup mm') (filter dontMap (Set.toList freeable))
          dontMap name = not ((List.isPrefixOf "input" name) || 
                              (List.isPrefixOf "output" name))
          mNew =
            case freeableAddrs of
              (Just as) -> freeAll m' (map fst as)
              Nothing   -> m'
      in r xs (mNew,mm')
mmIM' :: IML -> Memory -> MemMap -> (Memory, MemMap)
mmIM' im memory memmap = r im (memory,memmap)
  where
    r [] m = m
    r (x:xs) (m,mm) =
      let
          (m',mm') = process x m mm
           
          freeable = getFreeableSet x xs
          freeableAddrs = mapM (flip Map.lookup mm') (filter dontMap (Set.toList freeable))
          dontMap name = not ((List.isPrefixOf "input" name) || 
                              (List.isPrefixOf "output" name))
          mNew =
            case freeableAddrs of
              (Just as) -> m' -- freeAll m' (map fst as)
              Nothing   -> m'
      in r xs (mNew,mm')

    
process (SAllocate name size t,_) m mm = (m',mm') 
  where (m',addr) = allocate m size
        mm' = case Map.lookup name mm of
          Nothing -> Map.insert name (addr,t) mm
          (Just (a, t)) -> error $ "mmIm: " ++ name ++ " is already mapped to " ++ show a

    -- A tricky case.                      
--    process (SForAllBlocks n im,_) m mm = mmIM im m mm
    -- Another tricky case. 
process (SSeqFor _ n im,_) m mm = mmIM im m mm
process (SSeqWhile b im,_) m mm = mmIM im m mm 
    -- Yet another tricky case.
process (SForAll _ n im,_) m mm = mmIM im m mm
process (SDistrPar Warp n im,_) m mm = mmIM' im m mm -- mmIM im m mm
process (SDistrPar Block n im,_) m mm = mmIM im m mm 
    -- The worst of them all.
--    process (SForAllThreads n im,_) m mm = mmIM im m mm
--    process (SNWarps _ im,_) m mm = mmIM im m mm
--    process (SWarpForAll _ im,_) m mm = mmIM im m mm 

  --  process im m mm = error $ printStm im -- "process: WHat!"
process (_,_) m mm = (m,mm) 

-- Friday (2013 Mars 29, discovered bug) 
getFreeableSet :: (Statement Liveness,Liveness) -> IML -> Liveness 
getFreeableSet (_,l) [] = Set.empty -- not l ! 
getFreeableSet (_,l) ((_,l1):_) = l Set.\\ l1

---------------------------------------------------------------------------
-- Rename arrays in IM
--------------------------------------------------------------------------- 

renameIM :: MemMap -> IML -> IMList ()
renameIM mm im = zip (map (go . fst) im) (repeat ())
  where
    go (SAssign name ix e) = SAssign (renameIVar mm name)
                                     (map (renameIExp mm) ix)
                                     (renameIExp mm e)
    go (SAtomicOp name ix atop) = SAtomicOp (renameIVar mm name)
                                            (renameIExp mm ix)
                                            (renameAtOp mm atop) 
    go (SCond be im) = SCond (renameIExp mm be)
                             (renameIM mm im)
    go (SSeqFor str n im) = SSeqFor str (renameIExp mm n)
                                        (renameIM mm im)
    go SBreak = SBreak
    go (SSeqWhile n im) = SSeqWhile (renameIExp mm n)
                                    (renameIM mm im)
    go (SForAll lvl n im)   = SForAll lvl (renameIExp mm n)
                                          (renameIM mm im)
    go (SDistrPar lvl n im) = SDistrPar lvl (renameIExp mm n)
                                            (renameIM mm im) 

--    go (SForAllBlocks n im) = SForAllBlocks (renameIExp mm n)
--                                            (renameIM mm im)
--    go (SNWarps n im) = SNWarps (renameIExp mm n)
--                                (renameIM mm im)
--    go (SWarpForAll n im) = SWarpForAll (renameIExp mm n)
--                                        (renameIM mm im) 
    -- Strip this out earlier. 
    go (SAllocate name n t)  = SAllocate name n t 
    go (SDeclare name t) = SDeclare name t
    go SSynchronize      = SSynchronize 

---------------------------------------------------------------------------
-- Memory map the arrays in an CExpr
---------------------------------------------------------------------------
renameIExp mm e@(IVar nom t) =  renameIVar mm e 
renameIExp mm (IIndex (e1,es) t) = IIndex (renameIExp mm e1, map (renameIExp mm) es) t
renameIExp mm (IBinOp op e1 e2 t) = IBinOp op (renameIExp mm e1) (renameIExp mm e2) t
renameIExp mm (IUnOp op e t) = IUnOp op (renameIExp mm e) t 
renameIExp mm (IFunCall nom exprs t) = IFunCall nom (map (renameIExp mm) exprs) t
renameIExp mm (ICast e t) = ICast (renameIExp mm e) t
renameIExp mm (ICond e1 e2 e3 t) = ICond (renameIExp mm e1)
                                         (renameIExp mm e2)
                                         (renameIExp mm e3)
                                         t
renameIExp mm a = a


renameIVar mm (IVar name t) =
    case Map.lookup name mm of 
    Just (addr,t) -> 
      let core = sbaseIExp addr 
          cast c = ICast c t
      in cast core
    
    Nothing -> IVar name t
    where
      sbaseIExp 0    = IVar "sbase" (Pointer Word8) 
      sbaseIExp addr = IBinOp IAdd (IVar "sbase" (Pointer Word8)) 
                                   (IWord32 addr) 
                                   (Pointer Word8) 

renameAtOp mm AtInc = AtInc
renameAtOp mm (AtAdd e) = AtAdd (renameIExp mm e)
renameAtOp mm (AtSub e) = AtSub (renameIExp mm e)
renameAtOp mm (AtExch e) = AtExch (renameIExp mm e) 



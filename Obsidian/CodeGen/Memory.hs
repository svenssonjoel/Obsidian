
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
        mapMemory) 
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

------------------------------------------------------------------------------
-- Memory layout

type MemMap = Map.Map Name (Word32,Type)

type Address = Word32
type Bytes   = Word32 

data Memory = Memory {freeList  :: [(Address,Bytes)] ,
                      allocated :: [(Address,Bytes)] , 
                      size      :: Bytes} -- how much used
              
              
-- 48 kilobytes of smem              
sharedMem = Memory [(0,49152)] [] 0

updateMax :: Memory -> Memory 
updateMax mem = let m = maximum [a+b|(a,b) <- allocated mem]
                    m' = max m (size mem)
                in mem {size = m'}

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
   
                          
free :: Memory -> Address -> Memory
free m a = mem 
    where 
      bytes = lookup a (allocated m)
      al    = filter (\(addr,_) -> a /= addr) (allocated m)
      
      merge [] = [] 
      merge [x] = [x]
      merge ((x,b):(y,b2):xs) = if (x+b == y) then merge ((x,b+b2):xs) 
                                              else (x,b):merge((y,b2):xs)
      mem   = case bytes of 
                Nothing -> error $ "error: Address " ++ show a ++ " not found in free list"
                Just b -> m {freeList = compress ((a,b):(freeList m)),
                             allocated = al}

freeAll :: Memory -> [Address] -> Memory 
freeAll m [] = m
freeAll m (a:as) = freeAll (free m a) as

compress = merge . List.sort 

merge [] = [] 
merge [x] = [x]
merge ((x,b):(y,b2):xs) = if (x+b == y) then merge ((x,b+b2):xs) 
                           else (x,b):merge((y,b2):xs)





----------------------------------------------------------------------------
-- Map a program onto a memory

-- TODO: Make sure this does add any input or output arrays to the map
   
mapMemory :: Program Liveness -> Memory -> MemMap -> (Memory,MemMap) 
mapMemory = mapMemoryProgram 


mapMemoryProgram :: Program Liveness -> Memory -> MemMap -> (Memory,MemMap)    
mapMemoryProgram Skip m mm = (m,mm) 
mapMemoryProgram (Assign name i a) m mm  = (m,mm)
mapMemoryProgram (AtomicOp _ _ _ _) m mm = (m,mm)
-- Added Jan-21-2013
mapMemoryProgram (SeqFor nom n f) m mm = mapMemoryProgram (f (variable "X")) m mm 
mapMemoryProgram (ForAll n f) m mm = mapMemoryProgram (f (variable "X")) m mm       
-- Added Jan 2013
mapMemoryProgram (Cond c p) m mm = mapMemoryProgram p m mm 
mapMemoryProgram (Synchronize _) m mm = (m,mm)
mapMemoryProgram ((Allocate name size t alive) `ProgramSeq` prg2) m mm 
  = mapMemoryProgram prg2 {-m'-} mNew mm'
  where 
    (m'',addr) = allocate m size
    aliveNext  = whatsAliveNext prg2
    diff       = alive Set.\\ aliveNext
    diffAddr   = mapM (\x -> Map.lookup x mm') (filter dontMap {-(not . (List.isPrefixOf "input")-} (Set.toList diff))
    dontMap name = not ((List.isPrefixOf "input" name) || 
                        (List.isPrefixOf "output" name))
    mNew       =  
      case diffAddr of 
        (Just addys) -> freeAll m'' (map fst addys)
        Nothing      -> error $ "atleast one array does not exist in memorymap: " ++ show mm'
   
    -- TODO: maybe create Global arrays if Local memory is full.
   
    (m',mm') = 
      case Map.lookup name mm of 
        Nothing      -> (m'',Map.insert name (addr,t) mm)
        (Just (a,t)) -> (m,mm) -- what does this case really mean ? -
mapMemoryProgram (Allocate name size t _) m mm = (m',mm')
  where 
    (m'',addr) = allocate m size
    -- TODO: maybe create Global arrays if Local memory is full.
    -- t = Pointer$ Local$ typeOf$ getLLArray (head ws) `llIndex`  tid
    (m',mm') = 
      case Map.lookup name mm of 
        Nothing      -> (m'',Map.insert name (addr,t) mm)
        (Just (a,t)) -> (m,mm) -- what does this case really mean ? -
mapMemoryProgram (prg1 `ProgramSeq` prg2) m mm = mapMemoryProgram prg2 m' mm'
  where 
    (m',mm') = mapMemoryProgram prg1 m mm
mapMemoryProgram (Output n t) m mm = (m,mm) 
    


{-
mapMemoryProgram (Assign name i a) m mm = (m,mm) 
mapMemoryProgram (ForAll f n) m mm = mapMemoryProgram (f (variable "X")) m mm       
mapMemoryProgram (Cond c p) m mm = mapMemoryProgram p m mm 
mapMemoryProgram (Allocate name size t program) m mm = mapMemoryProgram program m' mm'
  where 
    (m'',addr) = allocate m size
    -- TODO: maybe create Global arrays if Local memory is full.
    -- t = Pointer$ Local$ typeOf$ getLLArray (head ws) `llIndex`  tid
    (m',mm') = 
      case Map.lookup name mm of 
        Nothing      -> (m'',Map.insert name (addr,t) mm)
        (Just (a,t)) -> (m,mm) -- what does this case really mean ? -
mapMemoryProgram (prg1 `ProgramSeq` prg2) m mm = mapMemoryProgram prg2 m' mm'
  where 
    (m',mm') = mapMemoryProgram prg1 m mm 

-}

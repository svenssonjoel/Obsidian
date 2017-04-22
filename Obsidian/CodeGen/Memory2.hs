
{- Joel Svensson 2012, 2013

   Notes:
     Jan-27-2015: Bug fix related to arrays alive when
                  entering into loops.
                  Fix seems to solve the problem.
                  Need to investigate that it still frees arrays
                  as soon as possible. 
     Nov-25-2014: Changes to memory management
  
     Jan-21-2013: Added a SeqFor case 

 -} 
module Obsidian.CodeGen.Memory2 
       (MemMap,
        Memory,
        allocate,
        free,
        freeAll,
        size, 
        Address,
        Bytes,
        memMapIM,
        renameIM,
        SharedMemConfig(..),
        createSharedMem
        ) 
       where 

import qualified Data.List as List
import qualified Data.Set  as Set
import Data.Word
import Data.Maybe 

import Obsidian.Types
import Obsidian.Globs

import Obsidian.Exp 
import Obsidian.CodeGen.Program
import Obsidian.CodeGen.Liveness

import Debug.Trace 


import qualified Data.Map as Map 
---------------------------------------------------------------------------
-- Planned improvements
---------------------------------------------------------------------------
-- # Always start a shared memory array at a "bank-aligned" address
--      + So that programmer can really effect access patterns. 
-- # Do not rename with pointers
--      instead output a list of (type ,name,address) quadruples
--      that can be used to create an alias (at top scope of program)
--      + Prettier code -> easier debugging 
--      + Potential efficiency issues, from less casting etc
-- # (LONG-TERM) Clever memory allocation
--      + The future is known! So encode the optimal memory
--        allocation schema

---------------------------------------------------------------------------
-- Memory layout
---------------------------------------------------------------------------

type MemMap = Map.Map Name (AlignedAddress,Type)

type AlignedAddress = (Address,Address) 

type Address = Word32
type Bytes   = Word32 

data Memory = Memory {freeList  :: [(Address,Bytes)] ,
                      allocated :: [(Address,Bytes)] , 
                      size      :: Bytes} -- how much used
            deriving Show 

updateMax :: Memory -> Memory 
updateMax mem = let m = maximum [a+b|(a,b) <- allocated mem]
                    m' = max m (size mem)
                in mem {size = m'}
              

---------------------------------------------------------------------------
-- Shared memory configurations
---------------------------------------------------------------------------
data SharedMemConfig =
  SharedMemConfig { smSize :: Bytes  -- amount of shared mem
                  , smBanks :: Word32   -- Number of banks 16/32
                  , smBankAlign :: Bool 
                  }


createSharedMem :: SharedMemConfig -> Memory
createSharedMem conf = Memory [(0,smSize conf)] [] 0 

-- bank allign an address, returning a new aligned address
-- and the number of EXTRA bytes that needs to be present
-- from the old provided address in order to store aligned data
-- at the new location.
bank_align :: SharedMemConfig -> Address -> (AlignedAddress, Bytes)
bank_align conf address =
  case (how_far_off == 0) of
    True -> ((address,address),0)
    False -> ((address,address + bump),  bump) 
    
  where banks = smBanks conf
        -- if address % bank_alignment == 0
        --   the address is aligned
        bank_alignment = banks * 4 -- number of banks * 4 bytes
        
        how_far_off = address `mod`  bank_alignment
        bump =  bank_alignment - how_far_off 

---------------------------------------------------------------------------
-- Allocate memory
--------------------------------------------------------------------------- 
allocate :: SharedMemConfig -> Memory -> Bytes -> (Memory,AlignedAddress)
allocate conf m b =
  case smBankAlign conf of
    True ->
       -- Does any memory location exist that
       -- allows for the allocation of this array 
       case catMaybes new_candidates of
         [] -> error $ "allocate: out of shared memory:" ++
                       "\n  Allocating: " ++ show b ++ " bytes" ++
                       "\n  Free List: " ++ show (freeList m) ++ 
                       "\n  Potentials: " ++ show address_candidates ++ 
                       "\n  Fit with align: " ++ show new_candidates 
         
         ((aligned_address,free_space,alloc_size):_) ->
           -- update free list
           -- Clear the allocated address from the free list 
           let fl  = filter (\(addr,_) -> (fst aligned_address /= addr)) (freeList m)
               -- if the chosen space is larger than what we need
               -- add the unused chunk to the free list 
               fl' = if alloc_size < free_space
                     then (fst aligned_address + alloc_size,
                           free_space - alloc_size):fl
                     else fl
           -- Update memory and return a result address
           in (updateMax $ m { freeList = fl'
                            , allocated =
                              (fst aligned_address,alloc_size):allocated m}
               , aligned_address)
              
    False ->
      case map (pretend_align b) address_candidates of
        [] -> error "out of shared memory"
         
        ((aligned_address,free_space,alloc_size):_) ->
           -- update free list
           -- Clear the allocated address from the free list 
           let fl  = filter (\(addr,_) -> (fst aligned_address /= addr)) (freeList m)
               -- if the chosen space is larger than what we need
               -- add the unused chunk to the free list 
               fl' = if alloc_size < free_space
                     then (fst aligned_address + alloc_size,
                           free_space - alloc_size):fl
                     else fl
           -- Update memory and return a result address
           in (updateMax $ m { freeList = fl'
                            , allocated =
                              (fst aligned_address,alloc_size):allocated m}
               , aligned_address)

      
                    

  where
    -- Candidates after aligning
    new_candidates = map (tryCandidate b) address_candidates
    -- Original address canditades 
    address_candidates = filter (\(_,y) -> y >= b) $ freeList m 
    -- Create silly AlignedAddress (that are not really aligned at all)
    pretend_align bytes (addr, free_space) = ((addr,addr),free_space,bytes) 

    -- try to align an address
    -- results in an AlignedAdress
    tryCandidate bytes (addr, free_space) =
      let (aligned_addr, extra_bytes) = bank_align conf addr
          alloc_size = bytes + extra_bytes
      in
       case free_space >= alloc_size of 
         True -> Just (aligned_addr,free_space,alloc_size)
         False -> Nothing 

---------------------------------------------------------------------------
-- Free memory 
---------------------------------------------------------------------------
free :: Memory -> AlignedAddress -> Memory
free m (alloc_addr,_) = mem 
    where 
      bytes = lookup (alloc_addr) (allocated m)
      al    = filter (\(addr,_) -> alloc_addr /= addr) (allocated m)

      -- TODO: Investigate this much closer.
      --       Is it a bug or is freeing a non allocated memory area
      --       OK?
      --  2014-Nov-25: I dont remember what this refers to
      --    But, if a problem resurfaces, look here. 
      
      mem   = case bytes of 
                Nothing -> m
                {-
                  error $ "error: Address " ++ show a ++
                          " not found in allocated list" ++
                          "\n" ++ show m
                -} 
                Just b -> m {freeList = compress ((alloc_addr,b):(freeList m)),
                             allocated = al}

freeAll :: Memory -> [AlignedAddress] -> Memory 
freeAll m [] = m
freeAll m (a:as) = freeAll (free m a) as


compress :: [(Address,Bytes)] -> [(Address,Bytes)]
compress = merge . List.sort
  where
    merge :: [(Address,Bytes)] -> [(Address,Bytes)]
    merge [] = [] 
    merge [x] = [x]
    merge ((x,b):(y,b2):xs) = if (x+b == y)
                              then merge ((x,b+b2):xs) 
                              else (x,b):merge((y,b2):xs)


---------------------------------------------------------------------------
-- Memory map the new IM
---------------------------------------------------------------------------

memMapIM :: SharedMemConfig -> IML -> MemMap -> (Memory, MemMap)
memMapIM conf im memmap = mmIM conf im memory memmap
  where
    memory = createSharedMem conf
  
mmIM :: SharedMemConfig -> IML -> Memory -> MemMap -> (Memory, MemMap)
mmIM conf im memory memmap = r im (memory,memmap)
  where 
    r [] m = m
    r (x:xs) (m,mm) =
      let
          (m',mm') = process conf x m mm
           
          freeable = getFreeableSet x xs
          freeableAddrs = mapM (flip Map.lookup mm') (filter dontMap (Set.toList freeable))
          dontMap name = not ((List.isPrefixOf "input" name) || 
                              (List.isPrefixOf "output" name))
          mNew =
            case freeableAddrs of
              (Just as) -> freeAll m' (map fst as)
              Nothing   -> m'
      in -- trace ("freeable: " ++ show freeable   ++ "\n") $ 
         r xs (mNew,mm')
    
    process :: SharedMemConfig -> (Statement Liveness,Liveness) -> Memory -> MemMap -> (Memory,MemMap)
    process conf (SAllocate name size t,_) m mm = (m',mm') 
      where (m',addr) = allocate conf m size
            mm' =
              case Map.lookup name mm of
                Nothing -> Map.insert name (addr,t) mm
                (Just (a, t)) -> error $ "mmIm: " ++ name ++ " is already mapped to " ++ show a

    -- Boilerplate
    -- BUG: Bug in memory management related to seqloops
    --      It may be better to try to fix this bug here.
    --      A special mmIM for the loop case may be needed. 
    process conf (SSeqFor _ n im,alive) m mm = mmIMLoop conf alive im m mm
    process conf (SSeqWhile b im,_) m mm = mmIM conf im m mm 
    process conf (SForAll _ n im,_) m mm = mmIM conf im m mm
    -- 2014-Nov-25:
    --   This one used mmIM' which was identical to mmIM.
    --   This must have been a leftover from when I thought
    --   warp memory needed some special attention here. 
    process conf (SDistrPar Warp n im,_) m mm =  mmIMDistrWarp conf im m mm
    process conf (SDistrPar Block n im,_) m mm = mmIM conf im m mm 
    process conf (_,_) m mm = (m,mm) 

-- Friday (2013 Mars 29, discovered bug)
-- 2014-Nov-25: was the "l" the bug ? (some details help)
getFreeableSet :: (Statement Liveness,Liveness) -> IML -> Liveness 
getFreeableSet (_,l) [] = Set.empty -- not l ! 
getFreeableSet (_,l) ((_,l1):_) = l Set.\\ l1


---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------
mmIMLoop conf nonfreeable im memory memmap = r im (memory,memmap)
  where 
    r [] m = m
    r (x:xs) (m,mm) =
      let
          (m',mm') = process conf x m mm
           
          freeable' = getFreeableSet x xs
          freeable  = freeable' Set.\\ nonfreeable
          freeableAddrs = mapM (flip Map.lookup mm') (filter dontMap (Set.toList freeable))
          dontMap name = not ((List.isPrefixOf "input" name) || 
                              (List.isPrefixOf "output" name))
          mNew =
            case freeableAddrs of
              (Just as) -> freeAll m' (map fst as)
              Nothing   -> m'
      in --trace ("freeable': " ++ show freeable' ++ "\n" ++
         --       "freeable: " ++ show freeable   ++ "\n" ++ 
         --       "nonfreeable: " ++ show nonfreeable) $
         r xs (mNew,mm')
    
    process :: SharedMemConfig -> (Statement Liveness,Liveness) -> Memory -> MemMap -> (Memory,MemMap)
    process conf (SAllocate name size t,_) m mm = (m',mm') 
      where (m',addr) = allocate conf m size
            mm' =
              case Map.lookup name mm of
                Nothing -> Map.insert name (addr,t) mm
                (Just (a, t)) -> error $ "mmIm: " ++ name ++ " is already mapped to " ++ show a

    -- Boilerplate
    process conf (SSeqFor _ n im,alive) m mm = mmIMLoop conf (nonfreeable `Set.union` alive) im m mm
    process conf (SSeqWhile b im,_) m mm = mmIMLoop conf nonfreeable im m mm 
    process conf (SForAll _ n im,_) m mm = mmIMLoop conf nonfreeable im m mm
    -- 2014-Nov-25:
    --   This one used mmIM' which was identical to mmIM.
    --   This must have been a leftover from when I thought
    --   warp memory needed some special attention here. 
    process conf (SDistrPar Warp n im,_) m mm = mmIMLoop conf nonfreeable im m mm 
    process conf (SDistrPar Block n im,_) m mm = mmIMLoop conf nonfreeable im m mm 
    process conf (_,_) m mm = (m,mm) 



-- NOTE: This is a hack to make programs distributed
--       over warps not "free" its arrays.
--       Distributing over warps introduces entirely new
--       shared memory behaviour..
--       This needs a review and some real thought!

mmIMDistrWarp conf im memory memmap = r im (memory,memmap)
  where 
    r [] m = m
    r (x:xs) (m,mm) =
      let
          (m',mm') = process conf x m mm
           
          freeable = getFreeableSet x xs
          --freeable  = freeable' Set.\\ nonfreeable
          freeableAddrs = mapM (flip Map.lookup mm') (filter dontMap (Set.toList freeable))
          dontMap name = not ((List.isPrefixOf "input" name) || 
                              (List.isPrefixOf "output" name))
          mNew =
            case freeableAddrs of
              (Just as) -> m' -- freeAll m' (map fst as)
              Nothing   -> m'
      in --trace ("freeable': " ++ show freeable' ++ "\n" ++
         --       "freeable: " ++ show freeable   ++ "\n" ++ 
         --       "nonfreeable: " ++ show nonfreeable) $
         r xs (mNew,mm')
    
    process :: SharedMemConfig -> (Statement Liveness,Liveness) -> Memory -> MemMap -> (Memory,MemMap)
    process conf (SAllocate name size t,_) m mm = (m',mm') 
      where (m',addr) = allocate conf m size
            mm' =
              case Map.lookup name mm of
                Nothing -> Map.insert name (addr,t) mm
                (Just (a, t)) -> error $ "mmIm: " ++ name ++ " is already mapped to " ++ show a

    -- Boilerplate
    process conf (SSeqFor _ n im,alive) m mm = mmIMDistrWarp conf  im m mm
    process conf (SSeqWhile b im,_) m mm = mmIMDistrWarp conf  im m mm 
    process conf (SForAll _ n im,_) m mm = mmIMDistrWarp conf  im m mm
    -- 2014-Nov-25:
    --   This one used mmIM' which was identical to mmIM.
    --   This must have been a leftover from when I thought
    --   warp memory needed some special attention here. 
    process conf (SDistrPar Warp n im,_) m mm = mmIMDistrWarp conf  im m mm 
    process conf (SDistrPar Block n im,_) m mm = mmIMDistrWarp conf im m mm 
    process conf (_,_) m mm = (m,mm) 


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
renameIExp :: MemMap -> IExp -> IExp 
renameIExp mm e@(IVar _ _) =  renameIVar mm e 
renameIExp mm (IIndex (e1,es) t) = IIndex (renameIExp mm e1, map (renameIExp mm) es) t
renameIExp mm (IBinOp op e1 e2 t) = IBinOp op (renameIExp mm e1) (renameIExp mm e2) t
renameIExp mm (IUnOp op e t) = IUnOp op (renameIExp mm e) t 
renameIExp mm (IFunCall nom exprs t) = IFunCall nom (map (renameIExp mm) exprs) t
renameIExp mm (ICast e t) = ICast (renameIExp mm e) t
renameIExp mm (ICond e1 e2 e3 t) = ICond (renameIExp mm e1)
                                         (renameIExp mm e2)
                                         (renameIExp mm e3)
                                         t
renameIExp _ a = a

renameIVar :: MemMap -> IExp -> IExp 
renameIVar mm (IVar name t) =
    -- t == t1 should be true 
    case Map.lookup name mm of 
    Just ((_,real_addr),t1) -> 
      let core = sbaseIExp (real_addr)
          cast c = ICast c t1
      in cast core
    
    Nothing -> IVar name t
    where
      sbaseIExp 0    = IVar "sbase" (Pointer Word8) 
      sbaseIExp addr = IBinOp IAdd (IVar "sbase" (Pointer Word8)) 
                                   (IWord32 addr) 
                                   (Pointer Word8)
renameIVar _ _ = error "renameIVar: incorrect expression" 

renameAtOp :: MemMap -> AtOp -> AtOp 
renameAtOp _ AtInc = AtInc
renameAtOp mm (AtAdd e) = AtAdd (renameIExp mm e)
renameAtOp mm (AtSub e) = AtSub (renameIExp mm e)
renameAtOp mm (AtExch e) = AtExch (renameIExp mm e) 



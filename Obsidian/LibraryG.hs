
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-} 

{- 2013
   Joel Svensson
   Niklas Ulvinge
-} 

module Obsidian.LibraryG where

import Obsidian.Array
import Obsidian.Program
import Obsidian.Exp
import Obsidian.Memory
import Obsidian.Library

import Control.Monad
import Data.Word

import Prelude hiding (zipWith)

---------------------------------------------------------------------------
-- Parallel concatMap  
---------------------------------------------------------------------------

pConcatMap f = pConcat . fmap f
pUnCoalesceMap f = pUnCoalesce . fmap f
pConcatMapJoin f = pConcat . fmap (pJoin.f)
pUnCoalesceMapJoin f = pUnCoalesce . fmap (pJoin.f)
pCoalesceMap n f = pUnCoalesce . fmap f . coalesce n
pSplitMap n f = pConcat . fmap f . splitUp n

---------------------------------------------------------------------------
-- Parallel Generate 
---------------------------------------------------------------------------

generate :: ASize l
              => l
              -> (EWord32 -> SPush t b)
              -> Push (Step t) l b
generate n f = pConcat $ fmap f $ iterations n 

---------------------------------------------------------------------------
-- Various concatenation
---------------------------------------------------------------------------

-- parallel concat of a pull of push 
pConcat :: ASize l => Pull l (SPush t a) -> Push (Step t) l a
pConcat arr =
  mkPush (n * fromIntegral rn) $ \wf ->
    forAll (sizeConv n) $ \bix ->
      let p = arr ! bix -- (Push _ p) = arr ! bix
          wf' a ix = wf a (bix * sizeConv rn + ix)
      in p <: wf'
  where
    n  = len arr
    rn = len $ arr ! 0

--                warpID
wConcat :: SPull (EWord32 -> SPush Warp a) -> SPush Block a
wConcat arr =
  mkPush (n * fromIntegral rn) $ \wf ->
     NWarps (fromIntegral n) $ \warpID -> 
        let p = arr ! warpID
            wf' a ix = wf a (warpID * sizeConv rn + ix)
        in (p warpID)  <: wf'
  where
    n  = len arr
    rn = len $ (arr ! 0) 0  -- bit awkward. 
    
         

-- sequential concatenation of a pull of push 
sConcat :: ASize l => Pull l (SPush t a) -> Push t l a
sConcat arr =
  mkPush (n * fromIntegral rn) $ \wf ->
  do
    seqFor (sizeConv n) $ \bix ->
      let p = arr ! bix -- (Push _ p) = arr ! bix
          wf' a ix = wf a (bix * sizeConv rn + ix)              
      in p <: wf'
  where 
    n  = len arr
    rn = len $ arr ! 0

-- pUnCoalesce adapted from Niklas branch. 
pUnCoalesce :: ASize l => Pull l (SPush t a) -> Push (Step t) l a
pUnCoalesce arr =
  mkPush (n * fromIntegral rn) $ \wf ->
  forAll (sizeConv n) $ \bix ->
    let p = arr ! bix
        wf' a ix = wf a (bix * sizeConv rn + ix)
    in p <: (g wf')
  where
    n  = len arr
    rn = len $ arr ! 0
    s  = sizeConv rn 
    g wf a i = wf a (i `div` s + (i`mod`s)*(sizeConv n))

---------------------------------------------------------------------------
-- load 
---------------------------------------------------------------------------
load :: ASize l => Word32 -> Pull l a -> Push Block l a 
load n arr =
  mkPush m $ \wf ->
  forAll (sizeConv n') $ \tid ->
  do
    seqFor (fromIntegral n) $ \ix -> 
      wf (arr ! (tid + (ix*n'))) (tid + (ix*n')) 

  where
    m = len arr
    n' = sizeConv m `div` fromIntegral n

store :: ASize l => Word32 -> Pull l a -> Push Block l a 
store = load 

---------------------------------------------------------------------------
-- Join (adapted from Niklas branch
---------------------------------------------------------------------------

pJoin ::  Program t (Push t s a) -> Push t s a
pJoin prg = mkPush n $ \wf -> do
  parr <- prg
  parr <: wf
  where n = len $ fst $ runPrg 0 prg 

pJoinPush :: (Pushable t, ASize s) => Program t (Pull s a) -> Push t s a
pJoinPush = pJoin . liftM push


-- wJoin for now.
wJoin ::  WProgram (Push Warp s a) -> EWord32 -> Push Warp s a
wJoin (WProgram prg) warpID = mkPush n $ \wf -> do
  parr <- (prg warpID) 
  parr <: wf
  where n = len $ fst $ runPrg 0 (prg warpID)


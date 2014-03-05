
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{- LANGUAGE UndecidableInstances -} -- remove

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

type family ElementType a
type instance ElementType (Pull l a) = a
type instance ElementType (Push t l a) = a
type instance ElementType (Program t (Push t l a)) = a
type instance ElementType (Program t (Pull l a)) = a 

class Concat p t | p -> t where
  pConcat :: ASize l => Pull l p -> Push (Step t) l (ElementType p)

instance Concat (Push t Word32 a) t where
  pConcat = pConcatP . fmap return
 
--instance Pushable t => Concat (Pull Word32 a) t where -- dangerous! 
--  pConcat arr = pConcatP (fmap (return . push) arr)

instance Concat (Program t (Push t Word32 a)) t where
  pConcat prg = pConcatP prg

instance Pushable t => Concat (Program t (Pull Word32 a)) t where
  pConcat prg = pConcatP (fmap (liftM push) prg) 


pConcatP :: ASize l => Pull l (Program t (SPush t a)) -> Push (Step t) l a
pConcatP arr =
  mkPush (n * fromIntegral rn) $ \wf ->
    forAll (sizeConv n) $ \bix ->
      let bp = arr ! bix -- (Push _ p) = arr ! bix
          wf' a ix = wf a (bix * sizeConv rn + ix)
          
      in do p <- bp 
            p <: wf'
  where
    n  = len arr
    rn = len $ fst $ runPrg 0 $ arr ! 0

                 
-- parallel concat of a pull of push 
--pConcat :: ASize l => Pull l (SPush t a) -> Push (Step t) l a
--pConcat arr =
--  mkPush (n * fromIntegral rn) $ \wf ->
--    forAll (sizeConv n) $ \bix ->
--      let p = arr ! bix -- (Push _ p) = arr ! bix
--          wf' a ix = wf a (bix * sizeConv rn + ix)
--      in p <: wf'
--  where
--    n  = len arr
--    rn = len $ arr ! 0

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
-- Join 
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


-- class PrgJoin prg t where
--   prgJoin :: prg (Push t s a) -> Push t s a


-- instance PrgJoin (Program t) t where
--   prgJoin = pJoin

-- instance PrgJoin WProgram Warp where
--   prgJoin (WProgram prg) = mkPush undefined $ \wf -> do -- WProgram do ? 
--     wid <- readWP
--     undefined
 

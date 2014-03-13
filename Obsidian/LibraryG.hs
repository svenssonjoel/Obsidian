
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
      

class  Concat p t | p -> t where
  pConcat ::  ASize l => Pull l p -> Push (Step t) l (ElementType p)

instance Concat (Push t Word32 a) t where
  pConcat = pConcatP . fmap return
 
--instance Pushable t => Concat (Pull Word32 a) t where -- dangerous! 
--  pConcat arr = pConcatP (fmap (return . push) arr)

instance Concat (Program t (Push t Word32 a)) t where
  pConcat prg = pConcatP prg

instance Pushable t => Concat (Program t (Pull Word32 a)) t where
  pConcat prg = pConcatP (fmap (liftM push) prg) 

-- ######################################################################
-- Experiment zone
-- ######################################################################
-- desired function
-- pConcat :: Pull l (Pull l1 a) -> Push ANYLEVEL l eltType
-- pConcat :: Pull l (Push t l1 a) -> Push (ANYLEVEL > level(t)) eltType
-- pConcat :: Pull l (Program t (Push t l1 a)) -> Push (ANYLEVEL > level(t)) eltType
-- pConcat :: Pull l (Program t (Pull l1 a)) -> Push (ANYLEVEL > level(t)) eltType
  
-- ######################################################################  
pConcatP :: ASize l => Pull l (Program t (SPush t a)) -> Push (Step t) l a
pConcatP arr =
  mkPush (n * fromIntegral rn) $ \wf ->
    distrPar (sizeConv n) $ \bix ->
      let bp = arr ! bix -- (Push _ p) = arr ! bix
          wf' a ix = wf a (bix * sizeConv rn + ix)
          
      in do p <- bp 
            p <: wf'
  where
    n  = len arr
    rn = len $ fst $ runPrg 0 $ core (arr ! 0) 0 -- core hack 

  
-- wConcat :: SPull (SPush Warp a) -> SPush Block a
-- wConcat arr =
--   mkPush (n * fromIntegral rn) $ \wf ->
--      Program $ \ warpid -> -- Here really awkward.
--                  -- I get a warpid from Program, and one from NWarps...
--                  -- And all because Force needs to know this id on the "outside"
--                  -- Nwarps is not really a loop! it should not proive a warpID.
--       NWarps (fromIntegral n) $ \_ -> ---warpID -> 
--         let p = arr ! (variable "warpID")
--             wf' a ix = wf a (variable "warpID" * sizeConv rn + ix)
--         in core (p  <: wf') (variable "warpID")  
--   where
--     n  = len arr
--     rn = len $ (arr ! 0)  -- bit awkward. 
    
         

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
  distrPar (sizeConv n) $ \bix ->
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
-- load :: ASize l => Word32 -> Pull l a -> Push Block l a 
-- load n arr =
--   mkPush m $ \wf ->
--   forAll (sizeConv n') $ \tid ->
--   do
--     seqFor (fromIntegral n) $ \ix -> 
--       wf (arr ! (tid + (ix*n'))) (tid + (ix*n')) 

--   where
--     m = len arr
--     n' = sizeConv m `div` fromIntegral n

-- store :: ASize l => Word32 -> Pull l a -> Push Block l a 
-- store = load 



---------------------------------------------------------------------------
-- Join 
---------------------------------------------------------------------------

pJoin ::  Program t (Push t s a) -> Push t s a
pJoin prg = mkPush n $ \wf -> Program $ \_ -> do
  parr <- core prg 0
  core (parr <: wf) 0 
  where n = len $ fst $ runPrg 0 (core prg 0) -- core hack  

pJoinPush :: (Pushable t, ASize s) => Program t (Pull s a) -> Push t s a
pJoinPush = pJoin . liftM push


-- wJoin for now.
-- wJoin ::  Program Warp (Push Warp s a) -> Push Warp s a
-- wJoin prg = mkPush n $ \wf -> Program $ \warpID -> do
  
--   parr <- core prg warpID -- (prg warpID) 
--   core (parr <: wf) warpID -- DUMMY 

--   where n = len $ fst $ runPrg 0 (core prg 0) -- DUMMY HACK 

-- class PrgJoin prg t where
--   prgJoin :: prg (Push t s a) -> Push t s a


-- instance PrgJoin (Program t) t where
--   prgJoin = pJoin

-- instance PrgJoin WProgram Warp where
--   prgJoin (WProgram prg) = mkPush undefined $ \wf -> do -- WProgram do ? 
--     wid <- readWP
--     undefined
 

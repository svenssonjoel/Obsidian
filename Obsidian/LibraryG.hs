
{-# LANGUAGE ScopedTypeVariables #-}

module Obsidian.LibraryG where

import Obsidian.Array
import Obsidian.Program
import Obsidian.Exp
import Obsidian.Memory

import Data.Word

---------------------------------------------------------------------------
-- Parallel concatMap  
---------------------------------------------------------------------------

pConcatMap f = pConcat . pMap f

---------------------------------------------------------------------------
--
--------------------------------------------------------------------------- 
pMap :: ASize l
         => (a -> Program t (SPush t b))
         -> Pull l a
         -> Pull l (SPush t b) 
pMap f as =
  mkPull n $ \bix -> 
    mkPush (fromIntegral rn) $
      \wf ->
      do 
        --(Push _ p) <- f (as ! bix)
        p <- f (as ! bix) 
        let wf' a ix = wf a (bix * sizeConv rn + ix)
        p <: wf'     
  where
    n = len as
    rn = len $ fst $ runPrg 0 (f (as ! 0))

-- Bug ? (may need ot tweak the wf) 
pConcat :: ASize l => Pull l (SPush t a) -> Push (Step t) l a
pConcat arr =
  mkPush (n * fromIntegral rn) $ \wf ->
  do
    forAll (sizeConv n) $ \bix ->
      let p = arr ! bix -- (Push _ p) = arr ! bix
      in p <: wf
  where
    n  = len arr
    rn = len $ arr ! 0


sConcat :: ASize l => Pull l (SPush t a) -> Push t l a
sConcat arr =
  mkPush (n * fromIntegral rn) $ \wf ->
  do
    seqFor (sizeConv n) $ \bix ->
      let p = arr ! bix -- (Push _ p) = arr ! bix
      in p <: wf
  where 
    n  = len arr
    rn = len $ arr ! 0
    
---------------------------------------------------------------------------
-- Parallel ZipWith 
---------------------------------------------------------------------------

pZipWith :: ASize l => (a -> b -> Program t (SPush t c))
           -> Pull l a
           -> Pull l b
           -> Pull l (SPush t c)
pZipWith f as bs =
  mkPull instances $ \ bix -> 
    mkPush (fromIntegral rn) $
    \wf ->
    do
      -- (Push _ p) <- f (as ! bix) (bs ! bix)
      p <- f (as ! bix) (bs ! bix) 
      let wf' a ix = wf a (bix * fromIntegral rn + ix) -- (bix * sizeConv n + ix)
      p <: wf'      

    where
      -- Is this ok?! (does it break?) 
      rn = len $ fst $ runPrg 0 (f (as ! 0) (bs ! 0))
   
      instances = min (len as) (len bs) 

---------------------------------------------------------------------------
-- Parallel Generate 
---------------------------------------------------------------------------
generate :: ASize l
              => l
              -> (EWord32 -> Program t (SPush t b))
              -> Push (Step t)  l b
generate n f =
    mkPush (n * fromIntegral inner) $ \wf ->
    forAll (sizeConv n) $ \tid ->
    do
      -- (Push _ p) <- f tid
      p <- f tid
      let wf' a ix = wf a (tid * fromIntegral inner + ix)
      p <: wf' 
    where
      inner = len $ fst  $ runPrg 0 ( f 0)     




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

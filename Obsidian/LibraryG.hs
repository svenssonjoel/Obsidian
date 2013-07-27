
{-# LANGUAGE ScopedTypeVariables #-}

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

---------------------------------------------------------------------------
-- Parallel mapping  
---------------------------------------------------------------------------

pConcatMap f = pConcat . pMap f
pUnCoalesceMap f = pUnCoalesce . pMap f
pConcatMapJoin f = pConcat . pMap (pJoin.f)
pUnCoalesceMapJoin f = pUnCoalesce . pMap (pJoin.f)
pCoalesceMap n f = pUnCoalesce . pMap f . coalesce n
pSplitMap n f = pConcat . pMap f . splitUp n

---------------------------------------------------------------------------
-- pMap and pZipWith
---------------------------------------------------------------------------

pMap :: ASize l
        => (a -> SPush t b)
        -> Pull l a
        -> Pull l (SPush t b)
pMap f as = mkPull (len as) $ \bix -> f (as ! bix)

pZipWith :: ASize l
            => (a -> b -> SPush t c)
            -> Pull l a
            -> Pull l b
            -> Pull l (SPush t c)
pZipWith f as bs =
  mkPull rn $ \bix -> f (as ! bix) (bs ! bix) 
  where rn = min (len as) (len bs) 


---------------------------------------------------------------------------
-- Parallel Generate 
---------------------------------------------------------------------------
generate :: ASize l
              => l
              -> (EWord32 -> SPush t b)
              -> Push (Step t) l b
generate n f =
    mkPush (n * fromIntegral inner) $ \wf ->
    forAll (sizeConv n) $ \tid ->
      let p = f tid
      in p <: wf 
    where
      inner = len $ f 0 



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

pJoin :: ASize s => Program t (Push t s a) -> Push t s a
pJoin prg = mkPush n $ \wf -> do
  parr <- prg
  parr <: wf
  where n = len $ fst $ runPrg 0 prg

--pJoin is not as nice here, since we have the t parameters. 

pJoinPush :: (Pushable t, ASize s) => Program t (Pull s a) -> Push t s a
pJoinPush = pJoin . liftM push


---------------------------------------------------------------------------
--
---------------------------------------------------------------------------

-- pMap :: ASize l
--          => (a -> Program t (SPush t b))
--          -> Pull l a
--          -> Pull l (SPush t b) 
-- pMap f as =
--   mkPull n $ \bix -> 
--     mkPush (fromIntegral rn) $
--       \wf ->
--       do 
--         --(Push _ p) <- f (as ! bix)
--         p <- f (as ! bix) 
--         p <: wf
--   where
--     n = len as
--     rn = len $ fst $ runPrg 0 (f (as ! 0))
--     -- m = len (as ! 0)

-- pZipWith :: ASize l => (a -> b -> Program t (SPush t c))
--            -> Pull l a
--            -> Pull l b
--            -> Pull l (SPush t c)
-- pZipWith f as bs =
--   mkPull instances $ \ bix -> 
--     mkPush (fromIntegral rn) $
--     \wf ->
--     do
--       -- (Push _ p) <- f (as ! bix) (bs ! bix)
--       p <- f (as ! bix) (bs ! bix) 
--       let wf' a ix = wf a (bix * fromIntegral rn + ix) -- (bix * sizeConv n + ix)
--       p <: wf'      

--     where
--       -- Is this ok?! (does it break?) 
--       rn = len $ fst $ runPrg 0 (f (as ! 0) (bs ! 0))
   
--       instances = min (len as) (len bs) 


-- generate :: ASize l
--               => l
--               -> (EWord32 -> Program t (SPush t b))
--               -> Push (Step t)  l b
-- generate n f =
--     mkPush (n * fromIntegral inner) $ \wf ->
--     forAll (sizeConv n) $ \tid ->
--     do
--       -- (Push _ p) <- f tid
--       p <- f tid
--       let wf' a ix = wf a (tid * fromIntegral inner + ix)
--       p <: wf' 
--     where
--       inner = len $ fst  $ runPrg 0 ( f 0)     


{-# LANGUAGE ScopedTypeVariables,
             FlexibleInstances,
             TypeSynonymInstances #-}

module Obsidian.LibraryG where

import Obsidian.Array
import Obsidian.Program
import Obsidian.Exp
import Obsidian.Memory

import Data.Word

---------------------------------------------------------------------------
-- Map Class 
---------------------------------------------------------------------------
class PMap t where
  pMap :: ASize l
         => (SPull a -> Program t (SPush t b))
         -> Pull l (SPull a)
         -> Push (Step t) l b

instance PMap (Step ()) where
  pMap threadf as =
    Push (n * fromIntegral rn) $
    \wf ->
    do
      forAll (sizeConv n) $ \tix -> do
        (Push _ p) <- threadf (as ! tix)
        let wf' a ix = wf a (tix * sizeConv rn + ix)
        p wf'      
    where
      n = len as
      rn = len $ fst $ runPrg 0 (threadf (as ! 0))
      m = len (as ! 0)
      
instance PMap (Step (Step ())) where
  pMap kern as =
    Push (blocks * fromIntegral rn) $
    \wf ->
    do
      forAllBlocks (sizeConv blocks) $ \bix -> do
        (Push _ p) <- kern (as ! bix)
        let wf' a ix = wf a (bix * sizeConv rn + ix)
        p wf'
    where
      blocks = len as
      rn = len $ fst $ runPrg 0 (kern (as ! 0))
      n = len (as ! 0)



---------------------------------------------------------------------------
-- ZipWith Class 
---------------------------------------------------------------------------
class PZipWith t where
  pZipWith :: ASize l => (SPull a -> SPull b -> Program t (SPull c))
           -> Pull l (SPull a)
           -> Pull l (SPull b)
           -> Push (Step t) l c

instance PZipWith (Step ()) where
  pZipWith threadf as bs =
    Push (threads * fromIntegral rn) $
    \wf ->
    do
      forAll (sizeConv threads) $ \tix -> do
        res <- threadf (as ! tix) (bs ! tix) 
        let (Push _ p) = push res
            wf' a ix = wf a (tix * sizeConv n + ix)
        p wf'      

    where
      -- Is this ok?! (does it break?) 
      rn = len $ fst $ runPrg 0 (threadf (as ! 0) (bs ! 0))
      n = min m k 

      m  = len (as ! 0)
      k  = len (bs ! 0)
      threads = min (len as) (len bs) 

instance PZipWith (Step (Step ())) where 
  pZipWith kern as bs =
    Push (blocks * fromIntegral rn) $
    \wf ->
    do
      forAllBlocks (sizeConv blocks) $ \bix -> do
        res <- kern (as ! bix) (bs ! bix)
        let (Push _ p) = push res
            wf' a ix = wf a (bix * sizeConv rn + ix)
        p wf'
    where
      -- Is this ok?! (does it break?) 
      rn = len $ fst $ runPrg 0 (kern (as ! 0) (bs ! 0))
      n = min m k
      -- assume uniformity
      m = len (as ! 0)
      k = len (bs ! 0)
      blocks = min (len as) (len bs) 


---------------------------------------------------------------------------
-- Generate Class  
---------------------------------------------------------------------------
class Generate t where
  generate :: ASize s
              => s
              -- Requires an SPush in the generator function. 
              -> (EWord32 -> Program t (SPush t b))
              -> Push (Step t)  s b


instance Generate Thread where
  generate n f =
    Push (n * fromIntegral inner) $ \wf ->
    forAll (sizeConv n) $ \tid ->
    do
      (Push _ p) <- f tid 
      let wf' a ix = wf a (tid * fromIntegral inner + ix)
      p wf' 
    where
      inner = len $ fst  $ runPrg 0 ( f 0)

instance Generate Block where
  generate n f =
    Push (n * fromIntegral inner) $ \wf ->
    forAllBlocks (sizeConv n) $ \bid ->
    do
      (Push _ p) <- f bid
      let wf' a ix = wf a (bid * fromIntegral inner + ix)
      p wf' 
    where
      inner = len $ fst  $ runPrg 0 ( f 0)             
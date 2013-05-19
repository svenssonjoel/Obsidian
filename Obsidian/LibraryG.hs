
{-# LANGUAGE ScopedTypeVariables #-}

module Obsidian.LibraryG where

import Obsidian.Array
import Obsidian.Program
import Obsidian.Exp
import Obsidian.Memory

import Data.Word

---------------------------------------------------------------------------
-- Parallel mapping  
---------------------------------------------------------------------------

pMap :: ASize l
         => (SPull a -> Program t (SPush t b))
         -> Pull l (SPull a)
         -> Push (Step t) l b
pMap f as = 
  Push (n * fromIntegral rn) $
    \wf ->
    do
      forAll (sizeConv n) $ \tix -> do
        (Push _ p) <- f (as ! tix)
        let wf' a ix = wf a (tix * sizeConv rn + ix)
        p wf'      
    where
      n = len as
      rn = len $ fst $ runPrg 0 (f (as ! 0))
      m = len (as ! 0)


-- Experimental 
pMap' :: ASize l
         => (SPull a -> Program t (SPush t b))
         -> Pull l (SPull a)
         -> Pull l (SPush t b) 
pMap' f as =
  mkPullArray n $ \bix -> 
    Push (fromIntegral rn) $
      \wf ->
      do 
        (Push _ p) <- f (as ! bix) 
        let wf' a ix = wf a (bix * sizeConv rn + ix)
        p wf'     
  where
    n = len as
    rn = len $ fst $ runPrg 0 (f (as ! 0))
    m = len (as ! 0)


---------------------------------------------------------------------------
-- Parallel ZipWith 
---------------------------------------------------------------------------

pZipWith :: ASize l => (SPull a -> SPull b -> Program t (SPush t c))
           -> Pull l (SPull a)
           -> Pull l (SPull b)
           -> Push (Step t) l c
pZipWith f as bs =
    Push (instances * fromIntegral rn) $
    \wf ->
    do
      forAll (sizeConv instances) $ \tix -> do
        (Push _ p) <- f (as ! tix) (bs ! tix) 
        let wf' a ix = wf a (tix * sizeConv n + ix)
        p wf'      

    where
      -- Is this ok?! (does it break?) 
      rn = len $ fst $ runPrg 0 (f (as ! 0) (bs ! 0))
      n = min m k 

      m  = len (as ! 0)
      k  = len (bs ! 0)
      instances = min (len as) (len bs) 


---------------------------------------------------------------------------
-- Parallel Generate 
---------------------------------------------------------------------------
generate :: ASize s
              => s
              -- Requires an SPush in the generator function. 
              -> (EWord32 -> Program t (SPush t b))
              -> Push (Step t)  s b
generate n f =
    Push (n * fromIntegral inner) $ \wf ->
    forAll (sizeConv n) $ \tid ->
    do
      (Push _ p) <- f tid 
      let wf' a ix = wf a (tid * fromIntegral inner + ix)
      p wf' 
    where
      inner = len $ fst  $ runPrg 0 ( f 0)     


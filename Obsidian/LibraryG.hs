
{-# LANGUAGE ScopedTypeVariables #-}
module Obsidian.LibraryG where

import Obsidian.Array
import Obsidian.Program
import Obsidian.Exp
import Obsidian.Memory


---------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------- 
mapG :: (SPull a -> BProgram (SPull b))
        -> DPull (SPull a)
        -> DPush Grid b
mapG kern as =
  Push (blocks * sizeConv n) $
  \wf ->
    do
      forAllBlocks blocks $ \bix -> do
        res <- kern (as ! bix)
        let (Push _ p) = push Block res
            wf' a ix = wf a (bix * sizeConv n + ix)
        p wf'
  where
    blocks = len as
    n = len (as ! 0)


mapT :: (SPull a -> TProgram (SPull b))
        -> SPull (SPull a)
        -> SPush Block b
mapT threadf as =
  Push (n * m) $
  \wf ->
    do
      forAll (sizeConv n) $ \tix -> do
        res <- threadf (as ! tix)
        let (Push _ p) = push Thread res
            wf' a ix = wf a (tix * sizeConv m + ix)
        p wf'      

  where
    n = len as
    m = len (as ! 0) 
---------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------- 
zipWithG :: (SPull a -> SPull b -> BProgram (SPull c))
           -> DPull (SPull a)
           -> DPull (SPull b)
           -> DPush Grid c
zipWithG kern as bs =
  Push (blocks * sizeConv n) $
  \wf ->
    do
      forAllBlocks blocks $ \bix -> do
        res <- kern (as ! bix) (bs ! bix)
        let (Push _ p) = push Block res
            wf' a ix = wf a (bix * sizeConv n + ix)
        p wf'
  where
    n = min m k
    -- assume uniformity
    m = len (as ! 0)
    k = len (bs ! 0)
    blocks = min (len as) (len bs) 
     

zipWithT :: (SPull a -> SPull b -> TProgram (SPull c))
        -> SPull (SPull a)
        -> SPull (SPull b) 
        -> SPush Block c
zipWithT threadf as bs =
  Push (threads * n) $
  \wf ->
    do
      forAll (sizeConv n) $ \tix -> do
        res <- threadf (as ! tix) (bs ! tix) 
        let (Push _ p) = push Thread res
            wf' a ix = wf a (tix * sizeConv n + ix)
        p wf'      

  where
    n = min m k 

    m  = len (as ! 0)
    k  = len (bs ! 0)
    threads = min (len as) (len bs) 
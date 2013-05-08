
{-# LANGUAGE ScopedTypeVariables #-}
module Obsidian.LibraryG where

import Obsidian.Array
import Obsidian.Program
import Obsidian.Exp
import Obsidian.Memory

import Data.Word


---------------------------------------------------------------------------
-- 
---------------------------------------------------------------------------
-- Same change as in genB (vs genB').
-- May give you the 2 elt per thread Power. 
mapG :: ASize l => (SPull a -> BProgram (SPull b))
        -> Pull l (SPull a)
        -> Push Grid l b
mapG kern as =
  Push (blocks * fromIntegral rn) $
  \wf ->
    do
      forAllBlocks (sizeConv blocks) $ \bix -> do
        res <- kern (as ! bix)
        let (Push _ p) = push Block res
            wf' a ix = wf a (bix * sizeConv rn + ix)
        p wf'
  where
    blocks = len as
    -- TODO: ensure this is not insane (runPrg functionality) 
    rn = len $ fst $ runPrg 0 (kern (as ! 0))
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
zipWithG :: ASize l => (SPull a -> SPull b -> BProgram (SPull c))
           -> Pull l (SPull a)
           -> Pull l (SPull b)
           -> Push Grid l c
zipWithG kern as bs =
  Push (blocks * fromIntegral rn) $
  \wf ->
    do
      forAllBlocks (sizeConv blocks) $ \bix -> do
        res <- kern (as ! bix) (bs ! bix)
        let (Push _ p) = push Block res
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
     

zipWithT :: (SPull a -> SPull b -> TProgram (SPull c))
        -> SPull (SPull a)
        -> SPull (SPull b) 
        -> SPush Block c
zipWithT threadf as bs =
  Push (threads * rn) $
  \wf ->
    do
      forAll (sizeConv threads) $ \tix -> do
        res <- threadf (as ! tix) (bs ! tix) 
        let (Push _ p) = push Thread res
            wf' a ix = wf a (tix * sizeConv n + ix)
        p wf'      

  where

    -- Is this ok?! (does it break?) 
    rn = len $ fst $ runPrg 0 (threadf (as ! 0) (bs ! 0))
    n = min m k 

    m  = len (as ! 0)
    k  = len (bs ! 0)
    threads = min (len as) (len bs) 

---------------------------------------------------------------------------
-- Experimental
---------------------------------------------------------------------------
-- zipWithG' :: forall a b c l. (ASize l, MemoryOps c)
--              => (SPull a -> SPull b -> BProgram (SPull c))
--              -> Pull l (SPull a)
--              -> Pull l (SPull b)
--              -> GProgram (Push Grid l c)
-- zipWithG' kern as bs =
--   do
--     snames <- forAllBlocks (sizeConv n) $ \bix ->
--       do
--         res <- kern (as ! bix) (bs ! bix)
--         let (Push _ p) = push Block res
--         p (assignArrayN n)
--     let pully = Pull blocks $ \bix -> (pullFromS snames n :: Pull Word32 c)
        
--     return $ Push (blocks * fromIntegral n) $
--       \wf ->
--       do
--         forAllBlocks (sizeConv blocks) $ \bix ->
--           forAll (sizeConv n) $ \tix ->
--             do
--               wf ((pully ! bix) ! tix) (bix * (sizeConv n) + tix)
      
--   where
--     n = min m k 
--     -- Assume uniformity
--     m = len (as ! 0)
--     k = len (bs ! 0)
--     blocks = (min (len as) (len bs))
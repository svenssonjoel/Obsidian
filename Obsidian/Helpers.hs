{-# LANGUAGE FlexibleContexts #-}

{- Joel Svensson 2012 -} 
module Obsidian.Helpers where 


import Obsidian.Array
import Obsidian.Exp 
import Obsidian.Program
import Obsidian.Sync

import Data.Bits
import Data.Word


----------------------------------------------------------------------------
--

pair :: Array Pull a -> Array Pull (a,a) 
pair arr = 
  mkPullArray n' (\ix -> (arr ! (ix*2),arr ! (ix*2+1))) 
  where 
    n = len arr
    n' = n `div` 2 

--unpair :: Array Pull (a,a) -> Array Push a 
{- 
unpair :: Pushy arr => arr (a,a) -> Array Push a 
unpair arr =  mkPushArray (\k -> parr !* (everyOther k))
         (2 * n)
  where 
    parr = push arr 
    n    = len parr
    everyOther f  = \(ix,(a,b)) -> f (ix * 2,a) *>* f (ix * 2 + 1,b)  
-}    
tripple :: Array Pull a -> Array Pull (a,a,a) 
tripple arr = mkPullArray n' (\ix -> (arr ! (ix*3),
                                      arr ! (ix*3+1),
                                      arr ! (ix*3+2)))  
  where 
    n = len arr
    n' = n `div` 3 

{- 
untripple :: Array Pull (a,a,a) -> Array Push a 
untripple arr =  mkPushArray (\k -> parr !* (everythrd k))
         (3 * n)
  where 
    parr = push arr 
    n    = len parr
    everythrd f  = \(ix,(a,b,c)) -> f (ix * 3,a) *>* f (ix * 3 + 1,b) *>* f (ix * 3+2, c)  
-} 

quad :: Array Pull a -> Array Pull (a,a,a,a) 
quad arr = mkPullArray  n' (\ix -> (arr ! (ix*4),
                                    arr ! (ix*4+1),
                                    arr ! (ix*4+2),
                                    arr ! (ix*4+3))) 
  where 
    n = len arr
    n' = n `div` 4 

{- 
unquad :: Array Pull (a,a,a,a) -> Array Push a 
unquad arr =  mkPushArray (\k -> parr !* (everythrd k))
         (4 * n)
  where 
    parr = push arr 
    n    = len parr
    everythrd f  = \(ix,(a,b,c,d)) -> f (ix * 4,a) *>* f (ix * 4 + 1,b) *>* f (ix * 4+2, c) *>* f (ix * 4+3,d)  
-}   

-- quint :: Array Pull a -> Array Pull (a,a,a,a) 
-- unquad :: Array Pull (a,a,a,a) -> Array Push a 

----------------------------------------------------------------------------
--

-- improve these by taking larger steps
-- Do ix+0, ix+n, ix+2n and so on
-- instead of ix*step+0, ix*step+1, ix*step+2 .. 
{-

-- TODO: Rethink these low level things

preload2 :: (Syncable (Array Push) a) => Array Pull a -> Kernel (Array Pull a) 
preload2 = pure pair ->- pure unpair ->- sync 

preload3 ::(Syncable (Array Push) a) => Array Pull a -> Kernel (Array Pull a) 
preload3 = pure tripple ->- pure untripple ->- sync 
 
preload4 ::(Syncable (Array Push) a) => Array Pull a -> Kernel (Array Pull a) 
preload4 = pure quad ->- pure unquad ->- sync 

-- Possibly more efficient on GPUs are: 
preload2' :: (Syncable (Array Push) a) => Array Pull a -> Kernel (Array Pull a) 
preload2' = pure pair' ->- pure unpair' ->- sync 

preload3' ::(Syncable (Array Push) a) => Array Pull a -> Kernel (Array Pull a) 
preload3' = pure tripple' ->- pure untripple' ->- sync 
 
preload4' ::(Syncable (Array Push) a) => Array Pull a -> Kernel (Array Pull a) 
preload4' = pure quad' ->- pure unquad' ->- sync 

----------------------------------------------------------------------------
--
push1, push2, push3, push4 :: Array Pull a -> Kernel (Array Push a)
push1 = pure push

push2 arr | len arr `mod` 2 /= 0 = error "push2: Array length is not multiple of two" 
          | otherwise            = pure (unpair' . pair') arr

push3 arr | len arr `mod` 3 /= 0 = error "push3: Array length is not multiple of three" 
          | otherwise            = pure (untripple' . tripple') arr 

push4 arr | len arr `mod` 4 /= 0 = error "push4: Array length is not multiple of four" 
          | otherwise            = pure (unquad' . quad') arr
---------------------------------------------------------------------------- 
-- 
pair' :: Array Pull a -> Array Pull (a,a) 
pair' arr = 
  mkPullArray (\ix -> (arr ! ix,arr ! (ix+ (fromIntegral n')))) n'
  where 
    n = len arr
    n' = n `div` 2 

--unpair :: Array Pull (a,a) -> Array Push a 
unpair' :: Pushy arr => arr (a,a) -> Array Push a 
unpair' arr =  mkPushArray (\k -> parr !* (everyOther k))
         (2 * n)
  where 
    parr = push arr 
    n    = len parr
    everyOther f  = \(ix,(a,b)) -> f (ix,a) *>* f (ix + (fromIntegral n),b)  
    
tripple' :: Array Pull a -> Array Pull (a,a,a) 
tripple' arr = mkPullArray (\ix -> (arr ! ix,
                                   arr ! (ix + fromIntegral n),
                                   arr ! (ix + fromIntegral (2*n)))) n' 
  where 
    n = len arr
    n' = n `div` 3 

untripple' :: Array Pull (a,a,a) -> Array Push a 
untripple' arr =  mkPushArray (\k -> parr !* (everythrd k))
         (3 * n)
  where 
    parr = push arr 
    n    = len parr
    everythrd f  = \(ix,(a,b,c)) -> f (ix,a) *>* 
                                    f (ix + (fromIntegral n),b) *>* 
                                    f (ix + (fromIntegral (2*n)), c)  
    
quad' :: Array Pull a -> Array Pull (a,a,a,a) 
quad' arr = mkPullArray (\ix -> (arr ! (ix),
                                arr ! (ix + fromIntegral n'),
                                arr ! (ix + fromIntegral (2*n')),
                                arr ! (ix + fromIntegral (3*n')))) n' 
  where 
    n = len arr
    n' = n `div` 4 

unquad' :: Array Pull (a,a,a,a) -> Array Push a 
unquad' arr =  mkPushArray (\k -> parr !* (everyfth k))
         (4 * n)
  where 
    parr = push arr 
    n    = len parr
    everyfth f  = \(ix,(a,b,c,d)) -> f (ix,a) *>* 
                                     f (ix + fromIntegral n,b) *>* 
                                     f (ix + fromIntegral (2*n),c) *>* 
                                     f (ix + fromIntegral (3*n),d)  




----------------------------------------------------------------------------
-- 
    
--ftof4 :: Array Pull (Exp Float) -> Array Push (Exp Float4) 
--ftof2 :: Array Pull (Exp Float) -> Array Push (Exp Float2) 
--f4tof :: Array Pull (Exp Float4) -> Array Pull (Exp Float)     
--f2tof :: Array Pull (Exp Float2) -> Array Pull (Exp Float) 
-} 
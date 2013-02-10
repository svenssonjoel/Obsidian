{- Joel Svensson 2012, 2013 
   Mary Sheeran  2012

   Notes:
   2013-01-08: Renamed GlobArray to GlobPush 
   2013-01-02: Added toGlobArray and toGlobArrayN
   2012-12-10: Refactoring
               (adherence to new Array types and program types)  
-}

{-# LANGUAGE FlexibleInstances,
             TypeSynonymInstances,
             ScopedTypeVariables,
             TypeFamilies,
             GADTs #-}

module Obsidian.Library where 

import Obsidian.Array 
import Obsidian.Exp 
import Obsidian.Program
import Obsidian.Types

import Obsidian.Force

-- needed for threadsPerBlock analysis 
import qualified Obsidian.CodeGen.Program as P 

import Data.Bits 
import Data.Word

import Prelude hiding (splitAt,zipWith,replicate)

---------------------------------------------------------------------------
-- Functor instance Pull/Push arrays
---------------------------------------------------------------------------
instance Functor Pull where 
  fmap f arr = Pull (len arr) $ \ix -> f (arr ! ix) 

instance Functor Push where
  fmap f (Push n pfun) =
    Push n $ \wf -> pfun (\a ix -> wf (f a) ix)

--instance Functor Distrib where
--  fmap f (Distrib bixf) = Distrib $ \bix -> f $ bixf bix

instance Functor GlobPush where
  fmap f (GlobPush wf ) =
    GlobPush
    $ \wf' -> wf (\a gix -> wf' (f a) gix)

instance Functor GlobPull where
  fmap f (GlobPull ixf) = GlobPull (f . ixf)

  

---------------------------------------------------------------------------
-- Reverse an array by indexing in it backwards
---------------------------------------------------------------------------
  
rev :: Pull a -> Pull a 
rev arr = mkPullArray n (\ix -> arr ! (m - ix))  
   where m = fromIntegral (n-1)
         n = len arr
         
---------------------------------------------------------------------------
-- splitAt (name clashes with Prelude.splitAt)
---------------------------------------------------------------------------
splitAt :: Integral i => i -> Pull a -> (Pull a, Pull a) 
splitAt n arr = (mkPullArray m (\ix -> arr ! ix), 
                 mkPullArray  (len arr - m) (\ix -> arr ! (ix + pos)))
  where pos = fromIntegral n
        m   = fromIntegral n


halve arr = splitAt n2 arr
  where 
    n = len arr
    n2 = n `div` 2

---------------------------------------------------------------------------
-- replicate 
---------------------------------------------------------------------------
replicate n a = mkPullArray n (\ix -> a)
replicateG a = GlobPull $ \ix -> a 

singleton a = replicate 1 a 

---------------------------------------------------------------------------
-- last
---------------------------------------------------------------------------

last arr = arr ! fromIntegral ( len arr - 1)


---------------------------------------------------------------------------
-- Shift arrays
---------------------------------------------------------------------------
shiftRight :: Choice a => Word32 -> a -> Pull a -> Pull a
shiftRight dist elt arr = resize (len arr)
                          $ replicate dist elt `conc` arr

-- TODO: incorrect! 
shiftLeft :: Choice a => Word32 -> a -> Pull a -> Pull a
shiftLeft dist elt arr = resize (len arr)
                         $ arr `conc`  replicate dist elt
                         
---------------------------------------------------------------------------
-- elements at even indices to fst output, odd to snd.
---------------------------------------------------------------------------
evenOdds :: Pull a -> (Pull a, Pull a)
evenOdds arr = (mkPullArray (n-n2) (\ix -> arr ! (2*ix)) ,
                mkPullArray n2     (\ix -> arr ! (2*ix + 1)))
  where
    n = fromIntegral (len arr)
    n2 = div n 2

---------------------------------------------------------------------------
-- Concatenate the arrays
---------------------------------------------------------------------------
conc :: Choice a => Pull a -> Pull a -> Pull a 
conc a1 a2 = mkPullArray (n1+n2)
               $ \ix -> ifThenElse (ix <* (fromIntegral n1)) 
                       (a1 ! ix) 
                       (a2 ! (ix - (fromIntegral n1)))
  where 
    n1 = len a1
    n2 = len a2 

    
---------------------------------------------------------------------------
-- zipp unzipp
---------------------------------------------------------------------------
unzipp :: Pull (a,b) -> (Pull a, Pull b)       
unzipp arr = (mkPullArray (len arr) (\ix -> fst (arr ! ix)) ,
              mkPullArray (len arr) (\ix -> snd (arr ! ix)) )
              
zipp :: (Pull a, Pull b) -> Pull (a, b)             
zipp (arr1,arr2) =  Pull (min (len arr1) (len arr2))
                      $ \ix -> (arr1 ! ix, arr2 ! ix) 

unzipp3 :: Pull (a,b,c) 
           -> (Pull a, Pull b, Pull c)       
unzipp3 arr = (fmap (\(x,_,_) -> x) arr,
               fmap (\(_,y,_) -> y) arr,
               fmap (\(_,_,z) -> z)  arr) 


zipp3 :: (Pull a, Pull b, Pull c) 
         -> Pull (a,b,c)             
zipp3 (arr1,arr2,arr3) = 
  mkPullArray (minimum [len arr1, len arr2, len arr3])
  (\ix -> (arr1 ! ix, arr2 ! ix, arr3 ! ix))
    


zipWith :: (a -> b -> c) -> Pull a -> Pull b -> Pull c
zipWith op a1 a2 =  
  mkPullArray (min (len a1) (len a2))
  (\ix -> (a1 ! ix) `op` (a2 ! ix))
                   

zipWithG :: (a -> b -> c) -> GlobPull a -> GlobPull b -> GlobPull c
zipWithG op a1 a2 = GlobPull $ \ix -> (a1 ! ix) `op` (a2 ! ix) 
                   
---------------------------------------------------------------------------
-- pair 
---------------------------------------------------------------------------
pair :: Pull a -> Pull (a,a)
pair (Pull n ixf) = 
  mkPullArray n' (\ix -> (ixf (ix*2),ixf (ix*2+1))) 
  where 
    n' = n `div` 2 



unpair :: Choice a => Pull (a,a) -> Pull a
unpair arr = 
    let n = len arr
    in  mkPullArray (2*n) (\ix -> ifThenElse ((mod ix 2) ==* 0) 
                                  (fst (arr ! (ix `shiftR` 1)))
                                  (snd (arr ! (ix `shiftR` 1)))) 


---------------------------------------------------------------------------
-- twoK (untested for proper functionality) 
---------------------------------------------------------------------------

binSplit = twoK

twoK ::Int -> (Pull a -> Pull b) -> Pull a -> Pull b 
twoK 0 f = f  -- divide 0 times and apply f
twoK n f =  (\arr -> 
              let arr' = mkPullArray lt (\i -> (f (mkPullArray  m (\j -> (arr ! (g i j)))) ! (h i))) 
                  m    = (len arr `shiftR` n)   --pow of two           
                  g i j = i .&. (fromIntegral (complement (m-1))) .|. j  
                  h i   = i .&. (fromIntegral (nl2-1))   -- optimize 

                  nl2   = (len (f (mkPullArray  m (\j -> arr ! variable "X"))))
                  lt    = nl2 `shiftL` n 
              in arr')
 

---------------------------------------------------------------------------
-- ***                          PUSHY LIBRARY                       *** ---
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- IxMap Class
---------------------------------------------------------------------------
class IxMap a where 
  ixMap :: (Exp Word32 -> Exp Word32) 
           -> a e 
           -> a e

instance IxMap Push where
  ixMap f (Push n p) = Push n (ixMap' f p)


instance IxMap Pull where 
  ixMap f (Pull n ixf) =  Pull n (ixf . f) 


instance IxMap GlobPull where 
  ixMap f (GlobPull ixf) =  GlobPull (ixf . f) 

-- like fmap but the function is applied to indices.. 
ixMap' :: (Exp Word32 -> Exp Word32) 
          -> ((a -> Exp Word32 -> TProgram ()) -> BProgram ()) 
          -> ((a -> Exp Word32 -> TProgram ()) -> BProgram ()) 
ixMap' f p = \wf -> p (\a ix -> wf a (f ix))


---------------------------------------------------------------------------
-- Concatenate on Push arrays 
---------------------------------------------------------------------------
concP :: (Pushable arr1,
          Pushable arr2) => (arr1 a, arr2 a) -> Push a     
concP (arr1,arr2) = 
  mkPushArray  (n1+n2)
  $ \wf ->
  do
    parr1 wf
    parr2 $ \a i -> wf a (fromIntegral n1 + i)
  
  where 
    (Push n1 parr1) = push arr1
    (Push n2 parr2) = push arr2


----------------------------------------------------------------------------
--
unpairP :: Pushable arr => arr (a,a) -> Push a
unpairP arr =
  Push n $ \k -> pushf (everyOther k)
  where
    parr@(Push n pushf) = push arr

everyOther :: (a -> Exp Word32 -> TProgram ()) 
              -> (a,a) -> Exp Word32 -> TProgram ()
everyOther wf (a,b) ix = wf a (ix * 2) >> wf b (ix * 2 + 1)  

---------------------------------------------------------------------------
-- zipP
---------------------------------------------------------------------------
zipP :: Pushable arr  => arr a -> arr a -> Push a  
zipP arr1 arr2 =
  Push (n1+n2)
  $ \func -> p1 (\a ix -> func a (2*ix)) >>
             p2 (\a ix -> func a (2*ix + 1))
         
  where 
    (Push n1 p1) = push arr1
    (Push n2 p2) = push arr2


---------------------------------------------------------------------------
-- ***                          GLOBAL ARRAYS                        *** --
---------------------------------------------------------------------------

-- A very experimenental instance of mapG 
mapG' :: (Pull a -> BProgram (Pull b))
        -> Word32
        -> GlobPull a
        -> GlobPush b
mapG' f n (GlobPull ixf)  =
  GlobPush 
        $ \wf ->
          ForAllBlocks 
           $ \bix ->
             do -- BProgram do block 
               let pully = Pull n (\ix -> ixf (bix * fromIntegral n + ix))

               let res' = f pully 
               res <- res' 
               let numThreads = P.threadsPerBlock $ P.convPrg res'
                   elemsPerThread = len res `div` numThreads

               if (numThreads == 0 || len res `mod` numThreads /= 0)
                 then 
                 ForAll (Just n) $ \ix -> wf (res ! ix) (bix * fromIntegral n + ix)
                 else
                 ForAll (Just n) $ \ix ->
                 sequence_ [wf (res ! (ix + fromIntegral (numThreads * j)))
                               (bix * fromIntegral n + (ix + fromIntegral (numThreads * j )))
                            | j <- [0..elemsPerThread]]


-- Old fasioned mapG
mapG :: (Pull a -> BProgram (Pull b))
        -> Word32 -- BlockSize ! 
        -> GlobPull a
        -> GlobPush b
mapG f n (GlobPull ixf)  =
  GlobPush 
        $ \wf ->
          ForAllBlocks 
           $ \bix ->
             do -- BProgram do block 
               let pully = Pull n (\ix -> ixf (bix * fromIntegral n + ix))
               res <- f pully
               ForAll (Just (len res)) $ \ix -> wf (res ! ix) (bix * fromIntegral n + ix)


-- I Think this one has more potential for generalisations. 
mapD :: (Pull a -> BProgram b)
        -> Word32
        -> GlobPull a
        -> DistPull b
mapD f n (GlobPull ixf) =
  DistPull $ \bix ->
    do
      let pully = Pull n (\ix -> ixf (bix * fromIntegral n + ix))
      f pully 

mapDist :: (Pull a -> BProgram b)
           -> Word32
           -> GlobPull a
           -> (Exp Word32 -> BProgram b)
mapDist f n (GlobPull ixf) bix =
  let pully = Pull n (\ix -> ixf (bix * fromIntegral n + ix))
  in  f pully 

---------------------------------------------------------------------------
-- See if having an Assignable/Allocable class is enough to generalise
-- the code below to pairs etc of Exp
---------------------------------------------------------------------------
-- Experimental (Improve upon this if it works)
-- I think this code looks quite horrific right now.
-- The potentially unnecessary assignments are pretty bad. 
class ToGProgram a where
  type Global a
  toGProgram :: (Exp Word32 -> BProgram a) -> GProgram (Global a)

instance Scalar a => ToGProgram (Pull (Exp a)) where
  type Global (Pull (Exp a)) = GlobPush (Exp a)
  toGProgram f =
    do      
      let (pulla,_) = runPrg 0 $ f (BlockIdx X)
      let n = len pulla 

      shared <- uniqueSM

      ForAllBlocks $ \bix ->
        do
          res <- f bix -- compute res.

          -- Sync
  
          Allocate shared (n * fromIntegral (sizeOf (undefined :: Exp a)))
                          (Pointer (typeOf (undefined :: Exp a)))

          ForAll (Just n) $ \tix ->
            -- potentially unnessecary assignment...
            -- if the last thing the local computation does is force. 
            Assign shared tix (res ! tix)


          Sync
          
      return $
        GlobPush $ \wf ->
        do
          ForAllBlocks $ \bix-> 
            ForAll (Just n) $ \tix ->
              wf (index shared tix)
                 (bix * fromIntegral n + tix)
        
                
                        
      
instance (Scalar a, Scalar b) => ToGProgram (Pull (Exp a), Pull (Exp b)) where
  type Global (Pull (Exp a),Pull (Exp b)) = (GlobPush (Exp a), GlobPush (Exp b))
  toGProgram f = 
    do      
      let ((pulla,pullb),_) = runPrg 0 $ f (BlockIdx X)
      let n1 = len pulla 
      let n2 = len pullb  


      shared1 <- uniqueSM
      shared2 <- uniqueSM 

      ForAllBlocks $ \bix ->
        do
          -- This is the heart of the matter. I want the f bix Program 
          --  to only occur once in the generated complete Program.
          (res1,res2) <- f bix -- compute results.

          --  Sync
      
          ------------------------------------------------------------------
          Allocate shared1 (n1 * fromIntegral (sizeOf (undefined :: Exp a)))
                           (Pointer (typeOf (undefined :: Exp a)))
                         
          ForAll (Just n1) $ \tix ->
            -- potentially unnessecary assignment...
            -- if the last thing the local computation does is force. 
            Assign shared1 tix (res1 ! tix)

          ------------------------------------------------------------------
          Allocate shared2 (n2 * fromIntegral (sizeOf (undefined :: Exp b)))
                   (Pointer (typeOf (undefined :: Exp b)))
  
          ForAll (Just n2) $ \tix ->
            -- potentially unnessecary assignment...
            -- if the last thing the local computation does is force. 
            Assign shared2 tix (res2 ! tix)

          Sync
            
      let gp1 = 
            GlobPush $ \wf ->
              do
                ForAllBlocks $ \bix-> 
                  ForAll (Just n1) $ \tix ->
                  wf (index shared1 tix)
                  (bix * fromIntegral n1 + tix) 

      let gp2 =
            GlobPush $ \wf ->
            do
              ForAllBlocks $ \bix-> 
                ForAll (Just n2) $ \tix ->
                wf (index shared2 tix)
                   (bix * fromIntegral n2 + tix)

          
      return (gp1,gp2)

--------------------------------------------------------------------------- 
        

-- The Problem is that I cannot share computations that
-- take place on the gridlevel (I think). 
-- Experiment
{- 
mapE :: forall a b . (Scalar a, Scalar b)
        => (Pull (Exp a) -> BProgram (Pull (Exp b)))
        -> Word32
        -> GlobPull (Exp a)
        -> GProgram (Pull (Exp b)) 
mapE f n (GlobPull ixf) =
  do


    shared <- uniqueSM 
    -- Allocate bytes in all shared memories and obtain a name
    
    --return undefined 
    ForAllBlocks $ \bix ->
      do 
        let pully = Pull n (\ix -> ixf (bix * fromIntegral n + ix))
        res <- f pully

        Allocate shared (n * fromIntegral (sizeOf (undefined :: Exp b)))
                       (Pointer (typeOf (undefined :: Exp b)) )
        ForAll (Just n) $ \ tid ->        
          do 
            Assign shared tid (res ! tid)
        
    return $ Pull 32 $ \ix -> (index shared (ix `mod` (fromIntegral n)))
-} 
  
 -- Assume Pull a here is one the special distributed pulls from above
{- 
pushBlocks :: Pull a -> GlobPush a
pushBlocks (Pull n ixf) =
  GlobPush $ \wf ->
     ForAllBlocks $ \bix ->
       ForAll (Just n) $ \tix -> wf (ixf tix) (bix * fromIntegral n + tix) 


experiment :: GlobPull (Exp Int) -> GProgram (Pull (Exp Int), Pull (Exp Int))
experiment input =
  do 
    arr <- mapE force 32 input
    return (arr, singleton (arr ! 31))

experiment2 :: GlobPull (Exp Int) -> GProgram (GlobPush (Exp Int), GlobPush (Exp Int))
experiment2 input =
  do
    (arr1,arr2) <- experiment input
    return (pushBlocks arr1, pushBlocks arr2)
-} 
                                 
{-
  I think something like mapE above is needed to expressed shared computations.
  Bad things about mapE is its very specific type and
  that it allocates a new shared memory array and potentially performs a
  completely unnecessary copy from the old shared memory array.

  To generalise mapE typeclasses are probably needed.

  A way to skip the unnecessary copy would be if it was possible
  to hand the name over to the local computation. "compute this and
  store the result here".
  This sounds like some notion of a mutable array...
  What would it mean if we could express such local computations ?

  sklansky :: Int -> (Exp a -> Exp a -> Exp a) -> Pull (Exp a) -> Name -> BProgram (Pull (Exp a) 
  sklansky 0 op arr res =
    forAllN (len arr) $ \ix ->
      Assign res ix (arr ! ix)
  sklansky n op arr res =
    let arr1 = twoK (n-1) (fan op) arr
    arr2 <- force arr1
    sklansky (n-1) op arr2 

  Not pure... But then, who says our dsl must be ? (isn't that part of the beauty
  of Haskell and embedded languages?). We are already in a wierd Program Monad
  and allow really dangerous push arrays... 

  Going even further then maybe force should take a mutable array and a
  push/pull array and they push the elements into the mutable array..
  This means that creation of mutable arrays must also be in the hands of
  the programmer.
  If array creation and force targets are in the hand of the programmer then
  in-placeness is also in the hands of the programmer + about a trillion
  new ways to shoot ones foot off. 

  I imagine that a mutable array could be simply a:
  data MArray a = MArray Word32 Name -- length and identifier.
-} 



-- mapG2 is really hard to get right. 
{- 
mapG2 :: (Pull a -> BProgram (Pull b, Pull c))
         -> Word32
         -> GlobPull a
         -> (GlobPush b, GlobPush c) -- hard to get this right!
                                     -- without repeating computations. 
mapG2 f n (GlobPull ixf) =
  (GlobPush
   $ \wf -> ForAllBlock
            $ \bix ->
            do
              let pully = Pull n (\ixf (bix * fromIntegral n + ix))
                  res = f pully 
   , GlobPush 
-} 

---------------------------------------------------------------------------
-- From Distributed array of blocks to a global push array
---------------------------------------------------------------------------
--toGlobPush :: Distrib (BProgram (Pull a))
--               -> GlobPush a               
--toGlobPush inp@(Distrib bixf) =
--  GlobPush bs $
--    \wf -> ForAllBlocks 
--           $ \bix ->
--           do -- BProgram do block 
--             arr <- bixf bix 
--             ForAll bs $ \ix -> wf (arr ! ix) bix ix 
--  where
--    -- Is this Ok?! 
--    bs = len $ fst $ runPrg 0 $ bixf 0

---------------------------------------------------------------------------
-- Create a global array that pushes to global
-- memory N elements per thread. 
--------------------------------------------------------------------------- 
--toGlobPushN :: Word32
--                -> Distrib (BProgram (Pull a))
--                -> GlobPush a
--toGlobPushN n dist =
--  GlobPush bs $ 
--  \wf -> ForAllBlocks 
--         $ \bix ->
--         do -- BProgram do block
--             arr <- getBlock dist bix
--             ForAll (bs `div` n) $
--               \ix ->
--                    sequence_ 
--                    -- correct indexing ? 
--                    [wf (arr ! (ix * n' + i')) bix (ix * n' + i')
--                   | i <- [0..n-1]
--                    , let n' = fromIntegral n
--                    , let i' = fromIntegral i]
--           
--                  
-- where
--    bs = len $ fst $ runPrg 0 $ getBlock dist 0 
--    -- nb = numBlocks dist

    
---------------------------------------------------------------------------
--
---------------------------------------------------------------------------

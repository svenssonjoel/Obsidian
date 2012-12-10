{-# LANGUAGE TypeOperators, 
             GADTs, 
             FlexibleContexts#-} 


module Obsidian.GCDObsidian.AddOn.SortLang where 


import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Sync
import Obsidian.GCDObsidian.Library

--temporary
import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA

import Control.Category
import Prelude hiding ((.),id)

import Data.Bits
import Data.Word


data a :-> b where 
  Pure  :: (Array a -> Array b) -> (Array a :-> Array b) 
  Two   :: (Array a :-> Array b) -> (Array a :-> Array b) 
  
-- TODO: Add 
-- Ilv
-- Vee
-- And whatever else is needed for sorting/merging

  Sync  :: Syncable a e => a e  :-> Array e 
  SLSeq :: (Array a :-> Array b) 
          -> (Array b :-> Array c) 
          -> (Array a :-> Array c) 
          
          
          
          
compile :: (a :-> b) -> a -> Kernel b
compile (Pure f)    = pure f
compile (Two p)     = compileTwo 1 p  
compile Sync        = sync  
compile (SLSeq f g) = compile f ->- compile g


compileTwo :: Int -> (Array a :-> Array b) -> Array a -> Kernel (Array b)
compileTwo n (Pure f) = pure$ twoK n f 
compileTwo n (SLSeq f g) = compileTwo n f ->- compileTwo n g 
compileTwo n Sync = sync 
compileTwo n (Two p) = compileTwo (n+1) p



test1 :: Array (Exp Int) :-> Array (Exp Int)
test1 = Two (Pure rev `SLSeq` Sync `SLSeq` Pure rev)
getTest1 = putStrLn$ CUDA.genKernel "test1" (compile test1) (namedArray "hej" 64) 


test2 :: Array (Exp Int) :-> Array (Exp Int)
test2 = Two (Pure rev `SLSeq` Sync `SLSeq` (Two (Pure rev)))
getTest2 = putStrLn$ CUDA.genKernel "test2" (compile test2) (namedArray "hej" 64) 

test3 :: Array (Exp Int) :-> Array (Exp Int)
test3 = Two (Pure rev `SLSeq` Sync `SLSeq` (Two (Pure rev   `SLSeq` Sync `SLSeq` (Two (Pure rev)))))
getTest3 = putStrLn$ CUDA.genKernel "test3" (compile test3) (namedArray "hej" 64) 


sklansky :: Int 
            -> (Exp Int -> Exp Int -> Exp Int) 
            -> (Array (Exp Int) :-> Array (Exp Int)) 
sklansky 0 op = Pure id
sklansky n op = Two (sklansky (n-1) op) `SLSeq`  Pure (fan op) 
                   `SLSeq` Sync

fan op arr = conc (a1, (fmap (op c) a2)) 
    where (a1,a2) = halve arr
          c       = a1 ! (fromIntegral (len a1 - 1))


getSklansky = putStrLn$ CUDA.genKernel "Sklansky" (compile (sklansky 3 (+))) (namedArray "hej" 8) 
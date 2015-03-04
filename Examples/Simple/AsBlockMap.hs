{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}


module AsBlockMap where

import Obsidian

import Prelude hiding (zip, reverse)
import qualified Prelude as P

import Data.Word


test :: SPull EWord32 -> SPush Block EWord32
test arr =
  exec $ do
    a <- compute arr
    b <- compute a 
    compute (fmap (+1) b)

test2 :: SPull EWord32 -> SPush Block EWord32
test2 arr =
  exec $ do
    let a = asBlockMap test (splitUp 32 arr) 
    b <- compute a
    let c = asBlockMap test (splitUp 64 b)
    d <- compute c 
    return $ test d
    
gridTest :: DPull EWord32 -> DPush Grid EWord32
gridTest arr = asGridMap test2 (splitUp 128 arr)

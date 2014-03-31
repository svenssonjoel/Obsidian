
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{- LANGUAGE UndecidableInstances -} -- remove

-- 2013 - 2014 Joel Svensson
-- 2013 Niklas Ulvinge

module Obsidian.LibraryG where

import Obsidian.Array
import Obsidian.Program
import Obsidian.Exp
import Obsidian.Memory
import Obsidian.Library

import Control.Monad
import Data.Word

import Prelude hiding (zipWith)



-- localPull :: (ASize s, Pushable t) => Program t (Pull s a) -> Push t s a
-- localPull = local . liftM push

-- localPull_ :: (ASize s, Pushable t)
--               => (a -> Program t (Pull s b)) -> a -> Push t s b
-- localPull_ f a = localPull (f a)

-- pJoin ::  Program t (Push t s a) -> Push t s a
-- pJoin prg = mkPush n $ \wf -> do
--   parr <- prg
--   parr <: wf
--   -- It is a bit scary that I need to "evaluate" programs here. 
--   where n = len $ fst $ runPrg 0 prg

-- pJoinPush :: (Pushable t, ASize s) => Program t (Pull s a) -> Push t s a
-- pJoinPush = pJoin . liftM push


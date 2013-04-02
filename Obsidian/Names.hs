

module Obsidian.Names where

import Obsidian.Globs

data Tree a = None
            | Single a
            | Tuple [Tree a]

type Names = Tree Name

instance Functor Tree where
  fmap f None = None
  fmap f (Single a) = Single $ f a
  fmap f (Tuple ts) = Tuple $ map (fmap f) ts
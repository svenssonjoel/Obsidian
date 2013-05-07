{-# LANGUAGE GADTs #-}

module Obsidian.Names where

import Obsidian.Globs

-- data Names = None
--           | Single Name
--           | Tuple [Names] 

data Names a where
  Single :: Name -> Names a
  Tuple  :: Names a -> Names b -> Names (a,b)
  Triple :: Names a -> Names b -> Names c -> Names (a,b,c) 

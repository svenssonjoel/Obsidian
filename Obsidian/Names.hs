{-# LANGUAGE GADTs #-}

module Obsidian.Names (Names(..), mapNamesM_) where

import Obsidian.Globs


data Names a where
  Single :: Name -> Names a
  Tuple  :: Names a -> Names b -> Names (a,b)
  Triple :: Names a -> Names b -> Names c -> Names (a,b,c) 

---------------------------------------------------------------------------
-- helpers
---------------------------------------------------------------------------
mapNamesM_ :: Monad m => (Name -> m ()) -> Names a -> m ()
mapNamesM_ f (Single nom)  = f nom
mapNamesM_ f (Tuple n1 n2) = mapNamesM_ f n1 >>
                             mapNamesM_ f n2
mapNamesM_ f (Triple n1 n2 n3) = mapNamesM_ f n1 >>
                                 mapNamesM_ f n2 >>
                                 mapNamesM_ f n3


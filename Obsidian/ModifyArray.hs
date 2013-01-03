
{- Joel Svensson,
   Josef Svenningsson
   2012 -}
module Obsidian.ModifyArray where

import Obsidian.Exp 
import Obsidian.Types
import Obsidian.Globs
import Obsidian.Program
import Obsidian.Array
import Obsidian.Atomic
import Obsidian.Library

import Data.Word

data Modify a = Modify { modFun   :: (Exp Word32 -> TProgram ()) -> BProgram ()
                       , atomicOp :: Atomic a 
                       , length   :: Word32 }

reverse :: Modify a -> Modify a
reverse (Modify ixf op l) = Modify ixf' op l
  where ixf' = \k -> ixf (k . rev)
        rev ix = Literal l - ix -1

instance IxMap Modify where
  ixMap f (Modify ixf op l) = Modify ixf' op l
    where ixf' k = ixf (k . f)

instance Len Modify where
  len (Modify _ _ l) = l

mkModifyArray :: ((Exp Word32 -> TProgram ()) -> BProgram ())
                 -> Atomic a -> Word32 -> Modify a
mkModifyArray ixf op l = Modify ixf op l

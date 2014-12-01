
module Obsidian.MultidimForce where


import Obsidian.Force
import Obsidian.Array
import Obsidian.MultiDim
import Obsidian.Memory
import Obsidian.Program 


computePullMD :: (IsStaticShape sh, Forceable t, Storable a) =>
                 PullMD sh a -> Program t (PullMD sh a) 
computePullMD (PullMD  origsh ixf) =
  do arr'  <- computePull arr
     return $ PullMD origsh (\ix -> arr' ! (flatIndex origsh ix))
  where
    s = staticSize origsh 
    arr = mkPull s (\ix -> ixf (unFlatIndex origsh ix))


computePushMD :: (IsStaticShape sh, Forceable t, Storable a) =>
                 PushMD t sh a -> Program t (PullMD sh a) 
computePushMD (PushMD origsh pf) =
  do arr' <- compute arr
     return $ PullMD origsh (\ix -> arr' ! (flatIndex origsh ix))
  where
    s = staticSize origsh
    arr = mkPush s $ \wf -> pf (\a shix -> wf a (flatIndex origsh shix))

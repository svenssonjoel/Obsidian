
{-

   sequential loops with state
   2013 : Joel Svensson 

-}

module Obsidian.SeqLoop where


import Obsidian.Program
import Obsidian.Exp
import Obsidian.Array 



---------------------------------------------------------------------------
-- Hacking, No real plan
---------------------------------------------------------------------------

seqFold :: (ASize l, Scalar a)
           => (Exp a -> Exp a -> Exp a)
           -> (Exp a)
           -> Pull l (Exp a)
           -> Program Thread (Exp a)
seqFold op init arr = do
  nom <- allocateLS init 
  Assign nom [] init  
  SeqFor n $ (\ ix ->
      Assign nom [] (variable nom `op`  (arr ! ix)))

    
  return $ variable nom
  where 
    n = sizeConv$ len arr
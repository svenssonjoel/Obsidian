{-# LANGUAGE FlexibleContexts #-}

{- Joel Svensson 2012 -} 
module Obsidian.Printing  where 

import Obsidian.Exp 
import Obsidian.Kernel 
import Obsidian.Array
import Obsidian.Program  


---------------------------------------------------------------------------
-- Print Code

printCode :: Show a => Program a -> IO () 
printCode c = putStrLn$ printProgram c

programToString :: Show a => Program a -> String 
programToString = printProgram
  
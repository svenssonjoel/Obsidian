{-# LANGUAGE FlexibleContexts #-}

module Obsidian.GCDObsidian.Printing  where 

import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Kernel 
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Program  


---------------------------------------------------------------------------
-- Print Code

printCode :: Show a => Program a -> IO () 
printCode c = putStrLn$ printProgram c

programToString :: Show a => Program a -> String 
programToString = printProgram
  
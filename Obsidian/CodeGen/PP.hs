
{- Joel Svensson 2012 -}
module Obsidian.CodeGen.PP where 

{- NOTES 2013-05-06: Apply Niklas Ulvinge's tweaks for speed -} 

import Control.Monad.State
import Data.Text (Text,pack,unpack,append,empty)
------------------------------------------------------------------------------
-- print and indent and stuff... 
--  This is probably very ugly 

-- TODO: There is a chapter about this pretty printing in "implementing functional lang..." 
--       Look at that and learn 


type PP a = State (Int,Text) a  

indent :: PP ()
indent = 
  do 
    (i,s) <- get 
    put (i+1,s) 
    
unindent :: PP () 
unindent = 
  do 
    (i,s) <- get 
    if i <= 0
      then error "PP.unindent: Indentation level messed up"
      else put (i-1,s) 

line :: String -> PP () 
line str = 
  do 
    (i,s) <- get 
    put (i,s `append` pack str) 

  
newline :: PP () 
newline = 
  do 
    (i,s) <- get 
    let ind = replicate (i*2) ' '
    put (i,s `append` pack ("\n" ++ ind))
    
runPP :: PP a -> Int -> String
runPP pp i = unpack $ snd $ execState pp (i,empty)

begin :: PP () 
begin = line "{" >> indent >> newline

end :: PP () 
end =  unindent >> newline >> line "}" >> newline

space   = line " " 
cTermLn = line ";" >> newline

wrap s e p = line s >> p >> line e 


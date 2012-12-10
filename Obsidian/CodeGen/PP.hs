module Obsidian.GCDObsidian.CodeGen.PP where 


import Control.Monad.State
------------------------------------------------------------------------------
-- print and indent and stuff... 
--  This is probably very ugly 

-- TODO: There is a chapter about this pretty printing in "implementing functional lang..." 
--       Look at that and learn 


type PP a = State (Int,String) a  

indent :: PP ()
indent = 
  do 
    (i,s) <- get 
    put (i+1,s) 
    
unindent :: PP () 
unindent = 
  do 
    (i,s) <- get 
    if i <= 0 then error "Whats going on" else put (i-1,s) 

line :: String -> PP () 
line str = 
  do 
    (i,s) <- get 
    put (i,s ++ str) 

  
newline :: PP () 
newline = 
  do 
    (i,s) <- get 
    let ind = replicate (i*2) ' '
    put (i,s ++ "\n" ++ ind)
    
runPP :: PP a -> Int -> String
runPP pp i = snd$ execState pp (i,"")

begin :: PP () 
begin = line "{" >> indent >> newline

end :: PP () 
end =  unindent >> newline >> line "}" >> newline

space   = line " " 
cTermLn = line ";" >> newline

wrap s e p = line s >> p >> line e 
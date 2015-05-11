
module Obsidian.Data where


import Obsidian.Exp
import Obsidian.Memory


--------------------------------------------------------------------------- 
-- Data (should match up with storable instances for completeness)
-- Use this to ensure nonnestednes where required 
---------------------------------------------------------------------------
class (Storable a, Choice a) => Data a
instance Scalar a => Data (Exp a)
instance (Data a, Data b) => Data (a,b)
instance (Data a, Data b, Data c) => Data (a,b,c)
instance (Data a, Data b, Data c, Data d) => Data (a,b,c,d) 
-- Storable has up to triples   

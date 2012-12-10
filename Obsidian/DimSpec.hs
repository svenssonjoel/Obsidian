
{- Joel Svensson 2012 -} 

module Obsidian.DimSpec (DimSpec (..)) where 

data DimSpec = X | Y | Z
             deriving (Eq,Ord,Show)

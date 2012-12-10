module Obsidian.Types where 


---------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------

data Type 
-- The allowed scalar types
  = Bool
  | Int  | Word        -- A bit problematic since the size of
                       -- of these are platform dependent
  | Int8 | Int16 | Int32 | Int64 
  | Word8 | Word16 | Word32 | Word64 
  | Float | Double                     
            
-- Used by CUDA, C And OpenCL generators          
  | Pointer Type   -- C thing 
  | Global Type    -- OpenCL thing
  | Local Type     -- OpenCL thing
  deriving (Eq, Show)
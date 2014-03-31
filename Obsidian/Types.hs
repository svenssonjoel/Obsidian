{-|
Module      : Types
Description : Type information, used internally by Obsídian.
Copyright   : (c) Joel Svensson, 2014
License     : BSD
Maintainer  : bo.joel.svensson@gmail.com
Stability   : experimental

-}

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
  | Volatile Type  -- For warp local computations. 
  | Pointer Type   -- Pointer to a @type@ 
  | Global Type    -- OpenCL thing
  | Local Type     -- OpenCL thing
  deriving (Eq, Ord, Show)
           
typeSize Int8 = 1
typeSize Int16 = 2
typeSize Int32 = 4
typeSize Int64 = 8
typeSize Word8 = 1
typeSize Word16 = 2
typeSize Word32 = 4
typeSize Word64 = 8
typeSize Bool = 4
typeSize Float = 4
typeSize Double = 8 

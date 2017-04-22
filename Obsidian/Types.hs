{-|
Module      : Types
Description : Type information, used internally by Obsídian.
Copyright   : (c) Joel Svensson, 2014, 2015
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

-- Vector types supported by CUDA (Add more)
  | FloatV2 | FloatV3 | FloatV4
  | DoubleV2

  -- | Int8V2 | Int8V3 | Int8V4
  -- | Int16V2 | Int16V3 | Int16V4
  -- | Int32V2 | Int32V3 | Int32V4
  -- | Int64V2

  -- | Word8V2 | Word8V3 | Word8V4
  -- | Word16V2 | Word16V3 | Word16V4
  -- | Word32V2 | Word32V3 | Word32V4
  -- | Word64V2

  | Int8 | Int16 | Int32 | Int64
  | Word8 | Word16 | Word32 | Word64
  | Float | Double
-- Vector Types
  | Vec2 Type | Vec3 Type | Vec4 Type


-- Used by CUDA, C And OpenCL generators
  | Shared Type
  | Volatile Type  -- For warp local computations.
  | Pointer Type   -- Pointer to a @type@
  | Global Type    -- OpenCL thing
  | Local Type     -- OpenCL thing
  deriving (Eq, Ord, Show)

typeSize :: Num a => Type -> a
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
typeSize (Shared t) = typeSize t
typeSize t = error $ "typeSize: this is bad!: " ++ show t

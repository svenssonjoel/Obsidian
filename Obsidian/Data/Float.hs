

module Obsidian.Data.Float where

-- For creation of literal "vector" types. 

{-
char1, uchar1 	1
char2, uchar2 	2
char3, uchar3 	1
char4, uchar4 	4
short1, ushort1 	2
short2, ushort2 	4
short3, ushort3 	2
short4, ushort4 	8
int1, uint1 	4
int2, uint2 	8
int3, uint3 	4
int4, uint4 	16
long1, ulong1 	4 if sizeof(long) is equal to sizeof(int) 8, otherwise
long2, ulong2 	8 if sizeof(long) is equal to sizeof(int), 16, otherwise
long3, ulong3 	4 if sizeof(long) is equal to sizeof(int), 8, otherwise
long4, ulong4 	16
longlong1, ulonglong1 	8
longlong2, ulonglong2 	16
float1 	4
float2 	8
float3 	4
float4 	16
double1 	8
double2 	16

-} 

data Float2 = Float2 Float Float
            deriving (Eq,Show) 
data Float3 = Float3 Float Float Float
            deriving (Eq,Show) 
data Float4 = Float4 Float Float Float Float 
            deriving (Eq,Show)  
data Double2 = Double2 Double Double
             deriving (Eq,Show) 

---------------------------------------------------------------------------
-- "constructors" 
---------------------------------------------------------------------------

float2 (a,b) = Float2 a b
float3 (a,b,c) = Float3 a b c
float4 (a,b,c,d) = Float4 a b c d 

double2 (a,b) = Double2 a b 

{-
   The cuda library contains constructors for these types.
   make_float2(float a, float b)
   make_float3(float a, float b, float c)
   ...

   Components are accessed as
   float4 f = ...
   float a = f.x;
   float b = f.y;
   float c = f.z;
   float d = f.w;

-} 

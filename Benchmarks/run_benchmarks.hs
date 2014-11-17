#!/usr/bin/env runghc
{-# LANGUAGE NamedFieldPuns #-}

-- | This script runs all Harlan benchmarks.  It is based on a Haskell
-- benchmarking framework called HSBencher.  Its main advantage is
-- that it supports uploading of benchmark timings to a Google Fusion
-- Table.

-- Requires hsbencher >= 0.2

import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import System.Directory
import System.FilePath
import System.Exit
import System.Environment (getArgs)
import System.Process
import GHC.Conc (getNumProcessors)
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace

import Data.Monoid (mappend)

import HSBencher
import HSBencher.Backend.Fusion  (defaultFusionPlugin)
import HSBencher.Backend.Dribble (defaultDribblePlugin)

import HSBencher.Types
import HSBencher.Internal.Logging (log)
import HSBencher.Internal.MeasureProcess
import HSBencher.Internal.Utils (runLogged, defaultTimeout)
import Prelude hiding (log)
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMainModifyConfig myconf

all_benchmarks :: [Benchmark DefaultParamMeaning]
all_benchmarks =
  [  mkBenchmark "ReductionBench/Reduce.cabal" [variant,cudaThreads,elems] defaultCfgSpc
  | variant     <- ["r1", "r2", "r3" ]
  , cudaThreads <- [ show (2^n) | n <- [5..10] ] -- 32 to 1024
  , elems       <- [ show (2^n) | n <- [8..14] ] -- 256 to 32768
  ] ++
  [  mkBenchmark "ReductionBench/Reduce.cabal" [variant,cudaThreads,elems] defaultCfgSpc
  | variant     <- ["r4", "r5", "r6", "r7" ]
  , cudaThreads <- [ show (2^n) | n <- [5..10] ] -- 32 to 1024
  , elems       <- [ show (2^n) | n <- [8..15] ] -- 256 to 32768
  ] ++
  [  mkBenchmark "ReductionBench/Reduce.cabal" [variant,kernel,cudaThreads] defaultCfgSpc
  | variant     <- ["large" ]
  , kernel      <- ["r1", "r2", "r3", "r4", "r5", "r6", "r7" ]
  , cudaThreads <- [ show (2^n) | n <- [5..10] ] -- 32 to 1024
  ] ++ 
  [  mkBenchmark "ScanBench/Scan.cabal" [variant,cudaThreads,elems] defaultCfgSpc
  | variant     <- ["s1", "s2", "s3", "k1", "k2" ]
  , cudaThreads <- [ show (2^n) | n <- [5..10] ] -- 32 to 1024
  , elems       <- [ show (2^n) | n <- [8..12] ] -- 256 to 4096    -- 32768
  ] ++
  [  mkBenchmark "ScanBench/Scan.cabal" [variant,reducer,scaner,cudaThreads,elems] defaultCfgSpc
  | variant     <- ["bigscan" ]
  , reducer     <- ["rbs_1", "rbs_2", "rbs_3", "rbs_4", "rbs_5", "rbs_6", "rbs_7"]
  , scaner      <- ["cin1", "cin2", "cin3", "cin4", "cin5"]
  , cudaThreads <- [ show (2^n) | n <- [5..10] ] -- 32 to 1024
  , elems       <- [ show (2^n) | n <- [8..12] ] -- 256 to 4096    -- 32768
  ] ++
  [  mkBenchmark "FractalBench/Fractals.cabal" [cudaThreads,size] defaultCfgSpc
  | cudaThreads <- [ show (2^n) | n <- [5..10] ] -- 32 to 1024
  , size       <- [ show (2^n) | n <- [8..13] ] -- 256x256 to 8192x8192
  ] ++
  [  mkBenchmark "GridSizeBench/GridSizeBench.cabal" [show num_blocks] defaultCfgSpc
  | num_blocks <- [16384, 8192, 4096, 2048, 1024, 512, 256, 128, 64, 32] -- 32 to 1024
  ]

-- | Default configuration space over which to vary settings:
--   This is a combination of And/Or boolean operations, with the ability
--   to set various environment and compile options.
defaultCfgSpc = And []
--  where
--   gpu = And [ Set (Variant "GPU") (RuntimeEnv "HARLAN_DEVICE" "gpu") ]


-- | Here we have the option of changing the HSBencher config
myconf :: Config -> Config
myconf conf =
  conf
   { benchlist = all_benchmarks
   , plugIns   = [ SomePlugin defaultFusionPlugin,
                   SomePlugin defaultDribblePlugin ]
   , harvesters =
       customTagHarvesterInt "ELEMENTS_PROCESSED" `mappend` 
       harvesters conf
   }

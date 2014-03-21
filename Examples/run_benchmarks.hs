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

import HSBencher (defaultMainModifyConfig)
import HSBencher.Types
import HSBencher.Methods (makeMethod)
import HSBencher.Logging (log)
import HSBencher.MeasureProcess
import HSBencher.Utils (runLogged, defaultTimeout)
import Prelude hiding (log)
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMainModifyConfig myconf

all_benchmarks :: [Benchmark DefaultParamMeaning]
all_benchmarks =
  [  mkBenchmark "ReductionTutorial/Reduce.cabal" [variant,cudaThreads,elems] defaultCfgSpc
  | variant     <- ["r1", "r2", "r3"]
  , cudaThreads <- [ show (2^n) | n <- [5..10] ] -- 32 to 1024
  , elems       <- [ show (2^n) | n <- [8..15] ] -- 256 to 32768
  ]

-- | Default configuration space over which to vary settings:
--   This is a combination of And/Or boolean operations, with the ability
--   to set various environment and compile options.
defaultCfgSpc = And []
  where
--   gpu = And [ Set (Variant "GPU") (RuntimeEnv "HARLAN_DEVICE" "gpu") ]


-- | Here we have the option of changing the HSBencher config
myconf :: Config -> Config
myconf conf =
  conf
   { benchlist = all_benchmarks
   }

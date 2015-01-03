#!/usr/bin/env runghc
{-# LANGUAGE NamedFieldPuns #-}

-- | This script runs all Obsidian benchmarks.

-- Requires hsbencher >= 0.2

import Data.Monoid (mappend)

import HSBencher
import HSBencher.Backend.Fusion  (defaultFusionPlugin)
import HSBencher.Backend.Dribble (defaultDribblePlugin)

import Prelude
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMainModifyConfig myconf

reduceConf1 = Or [And [ Set (Variant $ "r" ++ show v) (RuntimeArg  $ "r" ++ show v)
                      , Or [ Set NoMeaning (RuntimeArg (show (2^t))) | t <- [5..10]]  -- CUDA Threads 
                      , Or [ Set NoMeaning (RuntimeArg (show (2^n))) | n <- [8..14]]] -- Number of Elements  
                 | v <- [1,2,3]] 

reduceConf2 = Or [And [ Set (Variant $ "r" ++ show v) (RuntimeArg  $ "r" ++ show v)
                      , Or [ Set NoMeaning (RuntimeArg (show (2^t))) | t <- [5..10]]  -- CUDA Threads 
                      , Or [ Set NoMeaning (RuntimeArg (show (2^n))) | n <- [8..15]]] -- Number of Elements  
                 | v <- [4,5,6,7]] 

largeReduceConf = And [ Set (Variant "large") (RuntimeArg "large")
                      , Or [ Set NoMeaning (RuntimeArg kern) | kern <- ["r1", "r2", "r3", "r4", "r5", "r6", "r7" ]]
                      , Or [ Set NoMeaning (RuntimeArg (show (2^t))) | t <- [5..10]]]


specialReduceConf = And [ Set (Variant "SPECIAL") (RuntimeArg "SPECIAL")
                      , Or [ Set NoMeaning (RuntimeArg (show threads)) | threads <- [32,64,128,256,512,1024]]
                      , Or [ Set NoMeaning (RuntimeArg (show (2^t))) | t <- [23,24,25]]
                      , Or [ Set NoMeaning (RuntimeArg (show blocks))| blocks <- [16,32,64,128,256,512,1024]]]


scanConf = Or [ And [ Set (Variant v) (RuntimeArg v) 
                    , Or [ Set NoMeaning (RuntimeArg (show (2^t))) | t <- [5..10]]
                    , Or [ Set NoMeaning (RuntimeArg (show (2^n))) | n <- [8..12]]
                    ] | v <- ["s1", "s2", "s3", "k1", "k2" ]]
scan2Conf = Or [And [ Set (Variant v) (RuntimeArg v)
                    , Or [ Set NoMeaning (RuntimeArg (show blocks))]
                    , Or [ Set NoMeaning (RuntimeArg (show (16777216 `div` blocks)))]
                    , Or [ Set NoMeaning (RuntimeArg (show threads))] 
                    ]| blocks <- [16,32,64,128,256,512,1024]
                     , threads <- [32,64,128,256,512,1024]
                     , v <- ["chain1","chain2","chain3"]]

largeScan2Conf = Or [ And [ Set (Variant "bigscan2") (RuntimeArg "")
                          , Or [ Set NoMeaning (RuntimeArg iscan) | iscan <- ["iscan1", "iscan2", "iscan3", "iscan4", "iscan5"]]
                          , Or [ Set NoMeaning (RuntimeArg scan)  | scan  <- ["chain1", "chain2", "chain3"]]
                          , Or [ Set NoMeaning (RuntimeArg (show blocks))]
                            -- 8,16 and 32 M elements 
                          , Or [ Set NoMeaning (RuntimeArg (show (elts `div` blocks)))| elts <- [8388608,16777216,33554432]]
                          , Or [ Set NoMeaning (RuntimeArg (show threads))]
                          ] | blocks <- [16,32,64,128,256,512,1024]
                            , threads <- [32,64,128,256,512,1024]]

largeScanConf = And [ Set (Variant "bigscan") (RuntimeArg "bigscan")
                    , Or [ Set NoMeaning (RuntimeArg reducer) | reducer <- ["rbs_1", "rbs_2", "rbs_3", "rbs_4", "rbs_5", "rbs_6", "rbs_7"]]
                    , Or [ Set NoMeaning (RuntimeArg scaner)  | scaner  <- ["cin1", "cin2", "cin3", "cin4", "cin5"]]
                    , Or [ Set NoMeaning (RuntimeArg (show (2^t))) | t <- [5..10]]
                    , Or [ Set NoMeaning (RuntimeArg (show (2^n))) | n <- [8..12]]]

fractalConf = And [ Or [ Set NoMeaning (RuntimeArg (show (2^t))) | t <- [5..10]]
                  , Or [ Set NoMeaning (RuntimeArg (show (2^n))) | n <- [8..13]]]

gridSizeConf = Or [ And [ Set (Variant v) (RuntimeArg v)
                        , Or [Set NoMeaning (RuntimeArg (show blcks)) | blcks <- [16384, 8192, 4096, 2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1]]
                        ]| v <- [ "ITER_10"
                                , "ITER_100"                                  
                                , "ITER_1000"
                                , "ITER_10000"                                 
                                , "REDUCTION"
                                , "MAPCHAIN_NOSYNC_10"
                                , "MAPCHAIN_SYNC_10"
                                , "MAPCHAIN_NOSYNC_100"
                                , "MAPCHAIN_SYNC_100"
                                , "MAPCHAIN_NOSYNC_1000"
                                , "MAPCHAIN_SYNC_1000"
                                , "SKLANSKY"] ]

syncCostConf = And [ Set (Variant "SyncKern1") (RuntimeArg "SyncKern1")
                   , Or [ Set NoMeaning (RuntimeArg (show n_syncs)) | n_syncs <- [0..32]]
                   ]
                   
syncCostNWConf = Or [ And [ Set (Variant v) (RuntimeArg v)
                          , Or [Set NoMeaning (RuntimeArg (show num_warps)) | num_warps <- [1..32]]
                          ]| v <- ["SYNC","NOSYNC"]]

all_benchmarks :: [Benchmark DefaultParamMeaning]
all_benchmarks =
  [ (mkBenchmark "ReductionBench/Reduce.cabal" [] reduceConf1)  { progname = Just "Reduce" }
  , (mkBenchmark "ReductionBench/Reduce.cabal" [] reduceConf2)  { progname = Just "Reduce" } 
  , (mkBenchmark "ReductionBench/Reduce.cabal" [] largeReduceConf)  { progname = Just "Reduce" }
  , (mkBenchmark "ScanBench/Scan.cabal" [] scanConf) {progname = Just "Scan" }
  , (mkBenchmark "ScanBench2/Scan2.cabal" [] scan2Conf) {progname = Just "Scan2" }
  , (mkBenchmark "ScanBench2/Scan2.cabal" [] largeScan2Conf) {progname = Just "LargeScan2" }
  , (mkBenchmark "ScanBench/Scan.cabal" [] largeScanConf) {progname = Just "LargeScan"}
  , (mkBenchmark "FractalBench/Fractals.cabal" [] fractalConf) {progname = Just "Fractals"}
  , (mkBenchmark "GridSizeBench/GridSizeBench.cabal" [] gridSizeConf) {progname = Just "GridSize"} 
  , (mkBenchmark "SyncCostBench/SyncCostBench.cabal" [] syncCostConf) {progname = Just "SyncCost"} 
  , (mkBenchmark "SyncCostNumWarpsBench/SyncCostNumWarpsBench.cabal" [] syncCostNWConf) {progname = Just "SyncCostNumWarps"}
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
       customTagHarvesterDouble  "COMPILATION_TIME" `mappend`
       customTagHarvesterDouble  "TRANSFER_TO_DEVICE" `mappend`
       customTagHarvesterDouble  "TRANSFER_FROM_DEVICE" `mappend`
       customTagHarvesterInt "BYTES_TO_DEVICE" `mappend`
       customTagHarvesterInt "BYTES_FROM_DEVICE" `mappend`
       customTagHarvesterInt "CYCLES" `mappend`
       customTagHarvesterInt  "NUMBER_OF_BLOCKS" `mappend`
       customTagHarvesterInt "ELEMENTS_PER_BLOCK" `mappend`
       harvesters conf
   }

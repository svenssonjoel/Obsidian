#!/bin/bash

echo "Begin running jenkins benchmark script for Obsidian..."
set -x

# CONVENTION: The working directory is passed as the first argument.
CHECKOUT=$1
shift

if [ "$CHECKOUT" == "" ]; then
  CHECKOUT=`pwd`
fi
if [ "$JENKINS_GHC" == "" ]; then
  export JENKINS_GHC=7.6.3
fi
if [ -f "$HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_ghc.sh" ]; then
  source $HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_ghc.sh
fi
if [ -f "$HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_cuda.sh" ]; then
  source $HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_cuda.sh
fi

echo "Running benchmarks remotely on server `hostname`"

which cabal
cabal --version

which c2hs     || echo ok
c2hs --version || echo ok

unset GHC
unset GHC_PKG
unset CABAL

set -e

# Switch to where the benchmarks are
# ----------------------------------------
cd "$CHECKOUT"/Examples
rm -f run_benchmarks.exe
make run_benchmarks.exe

export TRIALS=1

# Parfunc account, registered app in api console:
CID=905767673358.apps.googleusercontent.com
SEC=2a2H57dBggubW1_rqglC7jtK

# Obsidian doc ID:  
TABID=1TsG043VYLu9YuU58EaIBdQiqLDUYcAXxBww44EG3
# https://www.google.com/fusiontables/DataSource?docid=1TsG043VYLu9YuU58EaIBdQiqLDUYcAXxBww44EG3

# Enable upload of benchmarking data to a Google Fusion Table:
# ./run_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload=$TABID --clientid=$CID --clientsecret=$SEC $*
./run_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload --name=Obsidian_bench_data --clientid=$CID --clientsecret=$SEC $*

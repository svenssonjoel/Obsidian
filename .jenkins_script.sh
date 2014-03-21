#!/bin/bash

set -e 
set -x 

source $HOME/rn_jenkins_scripts/acquire_ghc.sh

which -a cabal
cabal sandbox init 
cabal sandbox hc-pkg list

cabal install . 


cd Examples/Simple 
cabal sandbox init --sandbox=../../.cabal-sandbox
cabal install .


cd ../ReductionTutorial
cabal sandbox init --sandbox=../../.cabal-sandbox 
cabal install . 

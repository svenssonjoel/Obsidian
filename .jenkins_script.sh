#!/bin/bash

set -e 
set -x 

source $HOME/rn_jenkins_scripts/acquire_ghc.sh

which -a cabal
cabal sandbox init 
cabal sandbox hc-pkg list


# Need to pull down HsBencher
# ----------------------------------------
# Need to know what dir this is. and if we 
# are using cabal sandbox. 

echo Pull HSBencher from Github
git clone git@github.com:rrnewton/HsBencher 
cabal install ./HsBencher/hsbencher/ 
cabal install ./HsBencher/hsbnecher-fusion/ 



cabal update
cabal install . 




#cd Examples/Simple 
#cabal sandbox init --sandbox=../../.cabal-sandbox
#cabal install .


cd Examples/ReductionTutorial
cabal sandbox init --sandbox=../../.cabal-sandbox 
cabal install . 

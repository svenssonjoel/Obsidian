.phony: sand 
#default bench build_bench sand

# default: bench

# bench: build_bench 
# 	./run_benchmarks.exe
# Gah, Need to get HSBencher installed too! 
# Build the benchmarking script itself:
# build_bench: run_benchmarks.exe 

#NOTE! ALSO INSTALL OBSIDIAN HERE (easier to please cabal that way) 
run_benchmarks.exe: run_benchmarks.cabal run_benchmarks.hs sand
	cabal install --disable-documentation --disable-library-profiling ../ ../HSBencher/hsbencher/ ../HSBencher/hsbencher-fusion ../HSBencher/hgdata/ ../Obsidian.cabal -j --reinstall

	cabal install --bindir=. --program-suffix=.exe

DIR=$(shell pwd)

sand:
	echo "creating sandboxes under"$(DIR) 
#	(cd ..; cabal sandbox init)
#	cabal sandbox init --sandbox=../.cabal-sandbox/
	cabal sandbox init
	(cd ReductionBench; cabal sandbox init --sandbox=../.cabal-sandbox)
	(cd ScanBench;      cabal sandbox init --sandbox=../.cabal-sandbox) 
	(cd ScanBench2;     cabal sandbox init --sandbox=../.cabal-sandbox) 
	(cd FractalBench;   cabal sandbox init --sandbox=../.cabal-sandbox)
	(cd GridSizeBench;  cabal sandbox init --sandbox=../.cabal-sandbox)
	(cd SyncCostBench;  cabal sandbox init --sandbox=../.cabal-sandbox)
	(cd SyncCostNumWarpsBench;  cabal sandbox init --sandbox=../.cabal-sandbox)
# $(DIR)

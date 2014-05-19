.phony: default bench build_bench sand

default: bench

bench: build_bench 
	./run_benchmarks.exe

# Build the benchmarking script itself:
build_bench: run_benchmarks.exe 
run_benchmarks.exe: run_benchmarks.cabal run_benchmarks.hs sand
	cabal install --disable-documentation --disable-library-profiling ../ ../HSBencher -j --reinstall
	cabal install --bindir=. --program-suffix=.exe

DIR=$(shell pwd)

sand:
#	(cd ..; cabal sandbox init)
#	cabal sandbox init --sandbox=../.cabal-sandbox/
	cabal sandbox init
	(cd ReductionTutorial; cabal sandbox init --sandbox=$(DIR)/.cabal-sandbox/)
	(cd ScanBench;         cabal sandbox init --sandbox=$(DIR)/.cabal-sandbox/) 
	(cd FractalBench;      cabal sandbox init --sandbox=$(DIR)/.cabal-sandbox/)
	(cd Simple;            cabal sandbox init --sandbox=$(DIR)/.cabal-sandbox/)

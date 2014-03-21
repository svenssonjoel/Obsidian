.phony: default bench build_bench

default: bench

bench: build_bench
	./run_benchmarks.exe

# Build the benchmarking script itself:
build_bench: run_benchmarks.exe
run_benchmarks.exe: run_benchmarks.cabal run_benchmarks.hs
#	(cd ..; cabal sandbox init)
#	cabal sandbox init --sandbox=../.cabal-sandbox/
	cabal sandbox init
	(cd ReductionTutorial; cabal sandbox init --sandbox=../.cabal-sandbox/)
	(cd Simple; cabal sandbox init --sandbox=../.cabal-sandbox/)
	cabal install --disable-documentation --disable-library-profiling ../ ../HSBencher -j --reinstall
	cabal install --bindir=. --program-suffix=.exe

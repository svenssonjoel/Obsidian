.phony: default bench build_bench

default: build_bench

bench: build_bench

# Build the benchmarking script itself:
build_bench:
#	(cd ..; cabal sandbox init)
#	cabal sandbox init --sandbox=../.cabal-sandbox/
	cabal sandbox init
	cabal install --disable-documentation --disable-library-profiling ../ ../HSBencher -j 
	cabal install --bindir=. --program-suffix=.exe

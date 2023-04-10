build: configure
	@cabal build

configure:
	@cabal configure -f eclair-debug --enable-tests --enable-benchmarks
	@./benchmarks/cbits/prepare_benchmark.sh

clean:
	@cabal clean

test:
	@cabal run eclair-test
	# next line is broken on CI
	#@cabal run eclair-lsp-test
	@lit tests/ -v

bench: build
	@cabal bench > /dev/null
	@jq '.[2] | map({name: .reportName, unit: "Time (ms)", value: .reportAnalysis.anRegress[0].regCoeffs.iters.estPoint })' performance_results.json > performance_results_sorted.json
	@cat performance_results_sorted.json

.PHONY: build configure clean test bench

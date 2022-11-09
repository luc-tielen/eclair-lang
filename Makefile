build:
	@cabal build

configure:
	@cabal configure -f eclair-debug --enable-tests

clean:
	@cabal clean

test:
	@cabal run eclair-test
	@lit tests/ -v

.PHONY: build configure clean test

build:
	@cabal build

configure:
	@cabal configure -f eclair-debug --enable-tests

clean:
	@cabal clean

run:
	@cabal run

test:
	@cabal run eclair-test
	@lit tests/

repl:
	@cabal repl

.PHONY: build configure clean run test repl

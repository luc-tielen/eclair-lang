build: configure
	@cabal build

configure:
	@cabal configure -f eclair-debug --enable-tests

clean:
	@cabal clean

test:
	@DATALOG_DIR=cbits/ cabal run eclair-test
	@lit tests/ -v

cabal-file:
	@cabal-fmt --Werror -i eclair-lang.cabal

.PHONY: build configure clean test cabal-file

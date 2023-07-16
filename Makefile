build: configure
	@cabal build

configure:
	@cabal configure -f eclair-debug --enable-tests

clean:
	@cabal clean

test:
	@cabal run eclair-test
	# next line is broken on CI
	#@cabal run eclair-lsp-test
	@lit tests/ -v

cabal-file:
	@cabal-fmt --Werror -i eclair-lang.cabal

.PHONY: build configure clean test cabal-file

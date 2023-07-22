build: configure
	@cabal build

configure-stage0:
	@cabal configure --enable-tests -f stage0

configure-stage1:
	@cabal configure --enable-tests

# TODO switch to configure-stage1 when it works
configure: configure-stage0

clean:
	@cabal clean

test: configure
	@cabal run eclair-test
	# next line is broken on CI
	#@cabal run eclair-lsp-test
	@lit tests/ -v

cabal-file:
	@cabal-fmt --Werror -i eclair-lang.cabal

.PHONY: build configure configure-stage0 configure-stage1 clean test cabal-file

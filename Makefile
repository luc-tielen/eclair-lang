build:
	cabal build

configure:
	cabal configure --enable-tests --test-option=--color -fdebug

clean:
	rm -rf dist-newstyle/

run:
	cabal run eclairc

test:
	cabal test --test-show-details=direct

repl:
	cabal repl

.PHONY: build configure clean run test repl

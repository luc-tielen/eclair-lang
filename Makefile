build:
	stack build --fast

clean:
	stack clean

test:
	stack test --fast

repl:
	stack ghci

.PHONY: build clean test repl

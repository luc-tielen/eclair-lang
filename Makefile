build:
	stack build --fast

clean:
	stack clean

run:
	stack run

test:
	stack test --fast --flag eclair-lang:-debug

repl:
	stack ghci

.PHONY: build clean run test repl

# eclair

An experimental and minimal Datalog implementation, for better understanding of
certain compiler algorithms.

## Features

Eclair is a very minimal Datalog (for now). It only supports the following features:

- Facts containing literals (only integers are supported)
- Rules consisting of one or more clauses.
- Rules can be non-recursive, recursive or mutually recursive.

It's **extremely alpha** software, not ready for real use. Right now it only
compiles down to an intermediate representation.
Many edgecases are not checked yet (there is no semantic analysis / typesystem
yet).

## Roadmap

- [x] Compile Datalog to relation algebra (RA)
- [x] Prettyprinter for RA IR
- [x] Interpreter for RA IR
- [x] Add indices to relations
- [ ] Compile to LLVM
- [ ] Support other data types than integers (strings, ...)
- [ ] Add typesystem
- [ ] Add static analysis for the many edgecases
- [ ] ...

This roadmap is not set in stone, but it gives an idea on the direction of the
project. :smile:

## Why the name?

Eclair is inspired by [Soufflé](https://souffle-lang.github.io/), a high
performance Datalog that compiles to C++. Because of the similarities, I chose a
different kind of food that I like. I mean, an eclair contains *both* chocolate and
pudding, what's not to like!?


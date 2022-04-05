# eclair

An experimental and minimal Datalog implementation, for better understanding of
certain compiler algorithms.

## Features

Eclair is a very minimal Datalog (for now). It only supports the following features:

- Facts containing literals (only integers are supported)
- Rules consisting of one or more clauses.
- Rules can be non-recursive, recursive or mutually recursive.

It's **extremely alpha** software, not ready for real use. Right now it only
compiles down to an intermediate representation and is not end-to-end yet.
Many edgecases are not checked yet (there is no semantic analysis / typesystem
yet for example).

## Roadmap

- [x] Compile Datalog to relation algebra (RA)
- [x] Prettyprinter for RA IR
- [x] Interpreter for RA IR
- [x] Add indices to relations
- [x] Compile to Eclair IR (EIR)
- [ ] Compile to LLVM
- [ ] Clean up rough edges in the MVP
- [ ] Support other data types than integers (strings, ...)
- [ ] Add typesystem
- [ ] Add static analysis for edgecases
- [ ] ...

This roadmap is not set in stone, but it gives an idea on the direction of the
project. :smile:

## Why the name?

Eclair is inspired by [Soufflé](https://souffle-lang.github.io/), a high
performance Datalog that compiles to C++. Because of the similarities, I chose a
different kind of food that I like. I mean, an eclair contains *both* chocolate and
pudding, what's not to like!?

## Developer setup

## Nix

The easiest way to get the correct developer environment to build this project
is to use a recent enough Nix (with flakes support). The project also assumes
the following snippet is added to the `~/.direnvrc`:

```bash
use_flake() {
  watch_file flake.nix
  watch_file flake.lock
  mkdir -p $(direnv_layout_dir)
  eval "$(nix print-dev-env --profile "$(direnv_layout_dir)/flake-profile")"
}
```

Once this is done, you can just type the command `direnv allow` and Nix will do
the heavy lifting. After nix has finished setting up your local environment, you
can use `cabal` to build your project as usual. The Makefile contains the most
commonly used commands needed during development.

## Without Nix

You will need to install all tools yourself, manually. While this is possible,
it is not advised since you can potentially end up with a (slightly) different
environment, leading to weird to reproduce issues.

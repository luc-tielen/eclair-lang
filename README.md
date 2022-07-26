# eclair

An experimental and minimal Datalog implementation that compiles down to LLVM.

## Features

Eclair is a minimal Datalog (for now). It only supports the following features:

- Facts containing literals (only integers are supported)
- Rules consisting of one or more clauses.
- Rules can be non-recursive, recursive or mutually recursive.

It's **alpha** software, not ready for real use. Right now it compiles to LLVM
but expect there to be bugs. Some edge cases might not be checked yet.

## Motivating example

Let's say we want to find out which points are reachable in a graph. We can
determine which points are reachable using the following two logical rules:

1. One point is reachable from another point, iff there is a direct edge between
   those two points.
2. One point is reachable from another point, iff there is a third point 'z' such
   that there is a direct edge between 'x' and 'z', and between 'z' and 'y'.

The Eclair code below can be used to calculate the solution:

```eclair
@def edge(u32, u32).
@def reachable(u32, u32).

reachable(x, y) :-
  edge(x, y).

reachable(x, z) :-
  edge(x, y),
  reachable(y, z).
```

The above code can be compiled to LLVM using the Docker image provided by this repo:

```bash
$ git clone git@github.com:luc-tielen/eclair-lang.git
$ cd eclair-lang
$ docker build . -t eclair
# The next line assumes the eclair code is saved as "example.dl" in the current directory
$ docker run -v $PWD:/code --rm -it eclair:latest compile /code/example.dl
# NOTE: output can be redirected to a file using standard shell functionality: docker run ... > example.ll
```

This will emit the generated LLVM IR to the stdout of the terminal. If we save
this generated LLVM IR to a file (e.g. `example.ll`), we can link it with the
following C code that calls into Eclair, using the following command:
`clang -o program main.c example.ll`.

```c
// Save this file as "main.c".
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>


struct program;

extern struct program* eclair_program_init();
extern void eclair_program_destroy(struct program*);
extern void eclair_program_run(struct program*);
extern void eclair_add_facts(struct program*, uint16_t fact_type, uint32_t* data, size_t fact_count);
extern void eclair_add_fact(struct program*, uint16_t fact_type, uint32_t* data);
extern uint32_t* eclair_get_facts(struct program*, uint16_t fact_type);
extern void eclair_free_buffer(uint32_t* data);

int main(int argc, char** argv)
{
    struct program* prog = eclair_program_init();

    // edge(1,2), edge(2,3)
    uint32_t data[] = {
        1, 2,
        2, 3
    };
    eclair_add_facts(prog, 0, data, 2);

    eclair_program_run(prog);

    // NOTE: normally you call btree_size here to figure out the size, but I know there are only 3 facts
    uint32_t* data_out = eclair_get_facts(prog, 1);
    printf("REACHABLE: (%d, %d)\n", data_out[0], data_out[1]);  // (1,2)
    printf("REACHABLE: (%d, %d)\n", data_out[2], data_out[3]);  // (2,3)
    printf("REACHABLE: (%d, %d)\n", data_out[4], data_out[5]);  // (1,3)

    eclair_free_buffer(data_out);

    eclair_program_destroy(prog);
    return 0;
}
```

If you run the resulting program, this should print the reachable node pairs
`(1,2)`, `(2,3)` and `(1,3)` to the screen!

## Roadmap

- [x] Compile to LLVM
- [x] Release 0.0.1
- [x] Proper error handling
- [ ] LSP support
- [x] Support wildcards (`_`)
- [ ] Support other data types than integers (strings, ...), add typesystem
- [x] Support `=`
- [ ] Support negation, `!=` operator
- [ ] Support arithmetic / logical / ... operators
- [ ] Compile to WASM
- [ ] Optimizations on the AST / RA / LLVM level
- [ ] Support other underlying data structures than btree
- [ ] ...

This roadmap is not set in stone, but it gives an idea on the direction of the
project. :smile:

## Why the name?

Eclair is inspired by [Soufflé](https://souffle-lang.github.io/), a high
performance Datalog that compiles to C++. Because of the similarities, I chose a
different kind of food that I like. I mean, an eclair contains _both_ chocolate and
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
the heavy lifting. After Nix has finished setting up your local environment, you
can use `cabal` to build your project as usual. The Makefile contains the most
commonly used commands needed during development.

## Without Nix

You will need to install all tools yourself, manually. While this is possible,
it is not advised since you can potentially end up with a (slightly) different
environment, leading to weird to reproduce issues.

# Getting started

Eclair requires a Haskell toolchain, Souffle 2.3 and LLVM 14 to be installed on
your system.

If you notice that the installation instructions below are incomplete or
outdated, please open a [Github issue](https://github.com/luc-tielen/eclair-lang/issues).

## Pre-requisites

### Ubuntu

NOTE: These commands were tested with Ubuntu 20.04, they may not work with older
versions.

#### Installing the Haskell toolchain

Run the following commands to install `ghcup`, `ghc` and `cabal`. `hpack` and
`hspec-discover` are also installed but they are only needed when working on the
compiler.

```bash
$ curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
$ ghcup tui
# In the terminal UI, select GHC9.0.2, Haskell language server 1.8 and Cabal 3.6.
# Important: both install + set them!
$ cabal install hpack
$ cabal install hspec-discover
```

Verify you installed the correct versions by running the commands below, and
comparing them against the versions mentioned in the previous command:

```bash
$ ghc --version
$ haskell-language-server-wrapper --version
$ cabal --version
```

#### Installing Souffle

Run the following commands to download and build Souffle from source:

```bash
$ sudo apt install bison build-essential clang cmake doxygen flex g++ git \
  libffi-dev libncurses5-dev libsqlite3-dev make mcpp python sqlite zlib1g-dev

$ git clone git@github.com:souffle-lang/souffle.git
$ cd souffle
$ git checkout 2.3
$ cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
$ cmake --build build -j
$ sudo cmake --build build --target install
```

If this went correctly, Souffle should now be globally installed on your system.
Check this by executing the following command; it should print out the version
of Souffle (2.3).

```bash
$ souffle --version
```

#### Installing LLVM

Next we need to install LLVM 14. Run the steps below to install it on your
system.

```bash
$ sudo apt install llvm-14
$ sudo apt install clang-14  # Optional, if you want to use clang instead of llc
                             # to compile the LLVM IR
# The following is only needed for development / testing
$ cd ~/.local/bin  # Or any other directory that is on your $PATH
$ ln -s /usr/lib/llvm-14/bin/split-file
$ ln -s /usr/bin/FileCheck-14 FileCheck
$ pip install lit==14.0.6
```

### Windows

Assuming you have Windows subsytem for Linux, the commands above should also
work for Windows? (If somebody could verify this, that would be great!)

### OSX

NOTE: These commands were tested with Intel MacOS 13.0, they may or may not not work 
with older versions.

#### Installing the Haskell toolchain

Run the following commands (see https://www.haskell.org/ghcup/) to install `ghcup`, `ghc` and `cabal`. 


```bash
$ curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
$ ghcup tui
# In the terminal UI, select GHC9.0.2, Haskell language server 1.8 and Cabal 3.6.
# Important: both install + set them! 
```

`hpack` and `hspec-discover` are also installed but they are only needed when working on the compiler.

```bash
$ cabal install hpack
$ cabal install hspec-discover
```

Verify you installed the correct versions by running the commands below, and
comparing them against the versions mentioned in the previous command:

```bash
$ ghc --version
$ haskell-language-server-wrapper --version
$ cabal --version
```

#### Installing Souffle

Run the following commands to download and build Souffle from source (instructions taken from https://souffle-lang.github.io/build#mac-os-x-build):

```bash
$ brew update
$ brew install cmake bison libffi mcpp pkg-config
$ brew reinstall gcc
$ brew link bison --force
$ brew link libffi --force

$ git clone https://github.com/placidex/homebrew-souffle
$ cd homebrew-souffle/Formula
$ git checkout souffle-2.3
$ brew install --build-from-source ./souffle.rb
```

If this went correctly, Souffle should now be globally installed on your system.
Check this by executing the following command; it should print out the version
of Souffle (2.3).

```bash
$ souffle --version
```

#### Installing LLVM

Next we need to install LLVM 14. Run the steps below to install it on your
system.

```bash
$ brew install llvm@14
# The following is only needed for development / testing
$ cd ~/.local/bin  # Or any other directory that is on your $PATH
$ ln -s /usr/local/opt/llvm@14/bin/split-file
$ ln -s /usr/local/opt/llvm@14/bin/FileCheck
$ pip install lit==14.0.6
$ brew install node
```

## Building Eclair

Now that all the pre-requisites are built, you can now build Eclair.

```bash
$ cabal build
$ cabal run eclair-test  # Unit tests
$ lit tests -v           # Integration tests
$ cabal list-bin eclair  # <= returns path to the Eclair compiler executable
```

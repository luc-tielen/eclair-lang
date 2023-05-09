#!/bin/bash

LLVM_FLAGS=-O3

set -e

cd benchmarks/cbits
DATALOG_DIR=../../cbits/ cabal run eclair -v0 -- compile ./benchmark.eclair > ./benchmark.ll
llc-14 $LLVM_FLAGS -filetype=obj benchmark.ll -o benchmark.o
ar rcs libbenchmark.a benchmark.o

# Architecture choices

This document contains all the high level choices that have been made with
regards to the architecture of the compiler. Besides this document, there are
also [several blogposts](https://luctielen.com/) with deep dives on specific
parts of the compiler.

## Inspired by Souffle

Eclair's approach to compiling high level Datalog syntax to assembly is heavily
inspired by Souffle Datalog (most notably: the compilation to relational algebra
and the minimum index selection algorithm). At the top of the corresponding
source code in Eclair, there is a comment pointing to the paper so contributors
can consult the theoretic background behind the code easily.

Eclair does have some notable differences though. First and foremost: it is
written in Haskell. This choice was made because Haskell makes it easy to
express high level ideas and algorithms that you commonly run into when building
a compiler. As an additional benefit, Haskell already has a great ecosystem of
libraries for building a compiler.
The second big change compared to Souffle is that Eclair compiles down directly
to LLVM instead of C++. This gives greater control of the generated assembly
(assembly is generated using a monad / "builder pattern" instead of
concatenating strings to form a C++ program) and also makes it easily portable
to other platforms (including WebAssembly). On top of that we can leverage
existing LLVM tools to analyze / transform the generated LLVM IR.

## IR Design

Eclair makes use of four different intermediate representations (IRs). Each of
the IRs has a different focus / view of the program. By using multiple IRs, we
can also gradually lower the Datalog syntax to the assembly level.

The four different IRs are:

1. AST
2. RA
3. EIR
4. LLVM IR

Each of these IRs is discussed in the subsections below. Every IR can be pretty
printed for inspection (when debugging a compiler issue or when writing Eclair
code).

Each of the IRs are designed in a similar way: they all consist of a single data
type each. This might be a bit controversial for most Haskellers that value type
safety, but this ends up working out because:

1. The parser, semantic analysis and typesystem steps halt when they determine
   the program is invalid;
2. Often we are only interested in a really small part of the IR anyway;
3. Most Haskell libraries support one simple type best.

The singly-typed IR is a conciously made trade-off, but it gives us great
benefits. Transformations (a large part of the compiler!) can be written down
succinctly thanks to the
[recursion-schemes library](https://hackage.haskell.org/package/recursion-schemes).
The transforms are guaranteed to terminate when written this way, and are
automatically composable. Besides that, all algorithms can be written down as a
pattern-match that focuses on only one node of the IR.

Besides having singly-typed IRs, each data constructor in the IR type has a
unique node ID attached to it. This makes it possible to link data from outside
the IR to it, without having to keep modifying the IR over and over. This is
especially useful for the semantic analysis and typesystem parts of the
compiler, since they can refer to parts of the program via a node ID.

### AST

The first IR is the `AST` type. AST stands for "Abstract Syntax Tree" and is a
tree representation of the original source code.

Semantic analysis and typechecking happens on this IR before any transformations
are performed, so we can report back exact locations to users of Eclair.

The AST is the most "high level" / starting IR. AST compiles down to RA, which
is described in the next section.

### RA

`RA` stands for "Relational Algebra". It represents a Eclair Datalog program as
a set of relational algebra operations. The data type is pretty much copied
directly from the Souffle paper, with some minor modifications. By first
transforming the AST to RA, we can subsequently lower the code even further down
to the assembly level (via EIR and LLVM IR).

### EIR

`EIR` is an abbreviation for "Eclair IR". It is a IR designed to be very close
to LLVM IR, but with the focus that it is also easy to debug and inspect. It was
mainly created to make the final lowering to LLVM IR as trivial as possible, but
it ended up being also useful for stitching the various Eclair functions in the
runtime together.

### LLVM IR

The LLVM IR is the final IR this compiler makes use of and bears the closest
resemblance to assembly. The
[llvm-codegen library](https://github.com/luc-tielen/llvm-codegen) is used to
generate LLVM instructions. From this point onwards, we can make use of the LLVM
compiler framework to get many optimizations and other tools all for free.

## Query-based compiler

Eclair is a so-called "query-based compiler". What this means is:

1. Each stage of the compiler builds on top of previous stages,
2. You can query the results of each of these "sub-computations".

This kind of architecture ends up being very useful for a compiler since a
compiler often is not a "pipeline" as it is usually presented, but instead has
a graph structure where later stages can depend on earlier stages. On top of
that, it makes it simple to access information at each stage of the compiler,
making it easy to write tools that can query the compiler as a database. (Useful
for developer tools such as LSP!)

The [rock library](https://hackage.haskell.org/package/rock) builds on top of
this idea and provides an API for describing your compiler in terms of build
system rules.

## The parser

The parser is written using parser combinators (from the `megaparsec` library).
This approach was chosen because now this parser is fully written in Haskell,
making it trivial to integrate with the rest of the compiler. On top of that, it
gives you full control over how the parsing happens. In the Eclair compiler the
parser adds a unique node id to each parsed AST node (see section about IR
design).

## Semantic analysis

Eclair makes use of Souffle Datalog during semantic analysis. Using Datalog for
semantic analysis is great, because you can write all your logic really
succinctly by writing down the "patterns" you are looking for in the AST and let
Datalog deduce all results.

The fact that each AST node has a unique ID (see section about IR design) makes
it easy to refer to certain parts of a program and also to make it easy to
serialize data back and forth between Haskell and Datalog.

Eventually, Eclair will be a bootstrapped compiler, meaning all the parts where
Souffle Datalog is currently used will be swapped out with an Eclair Datalog
counterpart. This will make it much easier to distribute and run the compiler
(since there is one big dependency less required).

## Typesystem

The typesystem is a bidirectional typesystem. This means that the typesystem
either checks a term against an expected type, or it tries to infer a type.
Bidirectional typecheckers are great because they are "straight-forward" to
implement (you pretty much need to write two functions that pattern match on the
syntax and handle each node type correspondingly), and produce better error
messages than typesystems that make heavy use of constraint solving.

On top of this, the typesystem tries to report as many type errors as possible
at once and with additional context how it came to these conclusions. This is
done to make the developer experience better.

## Error rendering

An effort is made to make Eclair errors as clear as possible for developers
(using Rust and Elm for inspiration). Right now we use the
[diagnose library](https://github.com/Mesabloo/diagnose) for reporting the
errors since it allows us to focus on other parts of the compiler, but later
this error rendering system will be implemented in the Eclair codebase itself to
allow for more customization.

## Tranformations

Eclair has a general
[Transform](https://github.com/luc-tielen/eclair-lang/blob/main/lib/Eclair/Transform.hs)
type that can be used for transforming the various IRs. Transformations can have
two goals: they either simplify the IR, or they try to optimize it (or both).

Eclair is a nano-pass compiler. Transformations should be small, focused and
composable; so that you can reason about them. It's better to have a few extra
passes in the compiler instead of a lot of extra complexity. The `Transform`
type provides helper functions and typeclass instances to compose them into
bigger transforms anyway.

Transformations can have local state, but the way it is setup it is impossible
for this state to "leak out" to the outside world. This is again done to make it
easier to reason about, while not limiting what's possible inside a transform.

Finally, transforms always run in a `TransformM` monad. This monad offers a way
of generating new unique node IDs in case extra IR nodes are generated.

## Tests

Currently the Eclair test suite is divided into two parts:

1. Unit tests written directly in Haskell
2. "Integration" tests that are executed by the `lit` executable provided by LLVM.

Over time, most of the tests will be integration tests, since they are more
rigorous and test larger parts of the compiler. On top of that, these style of
tests allow you to write an example directly in Eclair and compare the result
against actual output of the compiler.

# Changelog

All notable changes to this project (as seen by library users) will be documented in this file.
The CHANGELOG is available on [Github](https://github.com/luc-tielen/souffle-haskell.git/CHANGELOG.md).

## [0.2.0] - Unreleased

### Added

- Typed hole support
- Comparison operators
- Arithmetic operators (`+`, `-`, `*`, `/`)
- Possibility to link in external functions
- LSP support
  - Document highlight
  - Hover
  - Diagnostics
- Improved dead code elimination
- Optimization passes:
  - HoistConstraints (faster searches by narrowing search-space as early as possible)

### Changed

- Relations now can have additional qualifiers marking them as inputs or
  outputs. Not providing any qualifier means it is now an internal fact.

### Fixed

- 0 is now parsed correctly as a number.

## [0.1.0] - 2022-11-20

### Added

- WebAssembly support
- Support for the `string` data type
- Wildcards are now supported in rule bodies
- Assignments are now supported in rule bodies
- Support for multiple occurences of the same variable in a single clause of
  a rule body
- (UTF-8) strings in relations are now supported
- Optimizations on the AST level:
  - Copy propagation
  - Dead code elimination

### Changed

- Improved error reporting
- Parsing now continues after failure and reports multiple errors back to the
  user at once.

### Fixed

- Rules with multiple equalities.
- Edgecase in index selection algorithm. The algorithm now does not take
  `NoElem` variants into account.

## [0.0.1] - 2022-06-14

### Added

- First MVP of the compiler! The happy path should work as expected, unsupported
  features or semantic errors should result in a (poorly formatted) error.

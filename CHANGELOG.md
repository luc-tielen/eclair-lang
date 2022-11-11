# Changelog

All notable changes to this project (as seen by library users) will be documented in this file.
The CHANGELOG is available on [Github](https://github.com/luc-tielen/souffle-haskell.git/CHANGELOG.md).

## [0.1.0] - Unreleased

### Added

- WebAssembly support
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

### Fixed

- Rules with multiple equalities.
- Edgecase in index selection algorithm. The algorithm now does not take
  `NoElem` variants into account.

## [0.0.1] - 2022-06-14

### Added

- First MVP of the compiler! The happy path should work as expected, unsupported
  features or semantic errors should result in a (poorly formatted) error.

# Changelog

All notable changes to this project (as seen by library users) will be documented in this file.
The CHANGELOG is available on [Github](https://github.com/luc-tielen/souffle-haskell.git/CHANGELOG.md).

## [0.1.0] - Unreleased

### Added

- Wildcards are now supported in rule bodies.
- Assignments are now supported in rule bodies.
- Support for multiple occurences of the same variable in one clause of rule
  bodies.

### Changed

- Improved error reporting.

### Fixed

- Edgecase in index selection algorithm. The algorithm now does not take
  `NoElem` variants into account.

## [0.0.1] - 2022-06-14

### Added

- First MVP of the compiler! The happy path should work as expected, unsupported
  features or semantic errors should result in a (poorly formatted) error.

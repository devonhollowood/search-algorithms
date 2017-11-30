# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

## [0.3.0] - 2017-11-29
### Added
- Monadic versions of search algorithms and helper functions

## [0.2.0] - 2017-05-13
### Changed
- BREAKING CHANGE: Simplified return type of `dijkstra` and `aStar`.
  - This should make these functions more ergonomic.
  - Introduced new `incrementalCosts` function to compensate.
- BREAKING CHANGE: Replaced searches' `prunes` arguments with `pruning` combinator.
- BREAKING CHANGE: Split searches' `next` arguments into multiple arguments for `dijkstra` and `aStar`.
  - This should make these functions more ergonomic.
- `next` arguments now only require a way of generating `Foldable`s, instead of lists specifically.

## 0.1.0 - 2017-03-07
- Initial release

[0.3.0]: https://github.com/devonhollowood/search-algorithms/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/devonhollowood/search-algorithms/compare/v0.1.0...v0.2.0

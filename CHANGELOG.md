# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

## [0.3.4] - 2025-06-28
- Add `dijkstraAssocCost` and `dijstraAssocCostM`, which make it easier to
  handle path-dependent costs.
- Fix bug in `dijkstraAssocM`, where the prior cost was not propagated to new states.

## [0.3.3] - 2024-11-08
- Add `pruningAssoc` and `pruningAssocM`, which allow for easy pruning of states based on cost.
- Add monadic versions of `dijkstraAssoc` and `astarAssoc`

## [0.3.2] - 2021-12-27
- Add two new functions, `dijkstraAssoc` and `aStarAssoc`. These allow for the simultaneous
  computation of neighboring states and their costs. (Thank you to
  [nagydani](https://github.com/nagydani))

## [0.3.1] - 2010-08-19
- Dependencies version bump

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

[0.3.4]: https://github.com/devonhollowood/search-algorithms/compare/v0.3.3...v0.3.4
[0.3.3]: https://github.com/devonhollowood/search-algorithms/compare/v0.3.2...v0.3.3
[0.3.2]: https://github.com/devonhollowood/search-algorithms/compare/v0.3.1...v0.3.2
[0.3.1]: https://github.com/devonhollowood/search-algorithms/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/devonhollowood/search-algorithms/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/devonhollowood/search-algorithms/compare/v0.1.0...v0.2.0

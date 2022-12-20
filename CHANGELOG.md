# Changelog for `filedo`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Support for additional rule properties
    - `environment` to set environment variables
    - `ignoreErrors` to ignore command errors and continue
    - `parallelise` to run commands for a rule in parallel
    - `useStdIn` to pipe a file from StdIn to the command

## [0.1.0] - 2022-12-22

The first release of `filedo`.

### Added

- `merge-data` command to merge data from multiple JSON and YAML sources
- `compile` command to substitute variables in a mustache template file
- `process` command to process a rule definition against a set of files

[Unreleased]: https://github.com/tvandinther/filedo/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/tvandinther/filedo/releases/tag/v0.1.0

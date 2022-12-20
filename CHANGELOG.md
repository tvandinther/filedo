# Changelog for `filedo`

## Unreleased

- Support for additional rule properties
    - `environment` to set environment variables
    - `ignoreErrors` to ignore command errors and continue
    - `parallelise` to run commands for a rule in parallel
    - `useStdIn` to pipe a file from StdIn to the command

## [0.1.0] - 2022-12-20

The first release of `filedo`.

### Added
- `merge-data` command to merge data from multiple JSON and YAML sources
- `compile` command to substitute variables in a mustache template file
- `process` command to process a rule definition against a set of files

[Unreleased]: https://github.com/tvandinther/filedo/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/tvandinther/filedo/releases/tag/v0.1.0

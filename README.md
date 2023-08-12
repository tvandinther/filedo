# filedo

**filedo** is a tool for running commands on files. The tool is comprised of three parts, each composed in the next.

---
### `merge-data`
```
filedo merge-data DATAFILES... [-o|--output OUTPUT] [-t|--type TYPE]

  Merge data files

Available options:
  DATAFILES...             Data files to merge. Last argument takes merge
                           precedence.
  -o,--output OUTPUT       Output file for merged data.
  -t,--type TYPE           Output type for merged data. (json, yaml)
  -h,--help                Show this help text
``` 
This command accepts many `JSON` or `YAML` files and merges them into a single file. The last file in the list takes precedence over the previous files. The output file can be specified with the `-o --output` flag. The output type can be specified with the `-t --type` flag. If no output type is specified, the output type will be inferred from the output file extension or default to `JSON`. If no output file is specified, the output will be printed to `stdout`.

---
### `compile`
```
filedo compile [-d|--data DATAFILES...] TARGET_DIRECTORY 
                          [-o|--output OUTPUTDIR] [-w]

  Compile mustache templates

Available options:
  -d,--data DATAFILES...   Data files to use. Last argument takes merge
                           precedence. Stdin will be used if none are specified.
  TARGET_DIRECTORY         Directory containing mustache templates
  -o,--output OUTPUTDIR    Directory for compiled template output. Default:
                           _build
  -w                       Suppress warnings.
  -h,--help                Show this help text
```
**filedo** can also compile and render [mustache](https://mustache.github.io/mustache.5.html) templates from a target directory. All files in the target directory are considered when resolving partials using their paths relative to the target directory.

This command composes the `merge-data` command to merge the data files specified with the `-d --data` option. Data can also be piped to `stdin`, and as such the following commands perform the same operations:
```
filedo compile ./templates -d data1.json data2.json

filedo merge-data data1.json data2.json | filedo compile ./templates
```
---
### `process`
```
filedo process [TARGET_DIRECTORY] [-r|--rule RULE_FILE] 
                          [-c|--compile] [-d|--data DATAFILES...] [-w]

  Process filedo rules

Available options:
  TARGET_DIRECTORY         Directory to process. Defaults to current directory.
  -r,--rule RULE_FILE      YAML file containing rules. Defaults to 'filedo.yaml'
  -c,--compile             Compile mustache templates before processing.
  -d,--data DATAFILES...   Data files to use. Last argument takes merge
                           precedence. Stdin will be used if none are specified.
  -w                       Suppress warnings.
  -h,--help                Show this help text
```
This is the main command of **filedo**. It parses a `YAML` file with the rule configuration and executes them. This command can operate either on a directory of compiled templates as if they were any other files, or templates can be compiled and used in-process with the `-c --compile` flag. If data is specified with the `-d --data` option, the rule configuration will be compiled in-process to substitute any values. This command is composed of the `merge-data` and `compile` commands.

---
## Rule YAML
The rule configuration file is a `YAML` file with the following structure:
```
Command = String | [String]
GlobPattern = String

Rule = Rule 
    { priority :: Integer
    , skip :: Bool
    , targets :: [GlobPattern]
    , exclude :: [GlobPattern]
    , useStdIn :: Bool
    , parallelise :: Bool
    , ignoreErrors :: Bool
    , pre :: Command
    , command :: Command
    , post :: Command
    , environment :: Map String String
    , rules :: [Rule] }
```
The top level object of the file is a `Rule`, with scoped subrules specified recursively in the `rules` field. This pattern, combined with all fields being optional with sensible defaults, allows for a very flexible and succint configuration.

| Field | Default | Description |
| --- | --- | --- |
| `priority` | `0` | The priority of the rule. Rules with higher priority are executed first. |
| `skip` | `false` | When true, this rule and its subrules will be skipped. |
| `targets` | `["*"]` | A list of glob patterns to match for target files. Patterns are scoped to the current target. |
| `exclude` | `[]` | A list of glob patterns to exclude from matches against the target file. Patterns are scoped to the current target. |
| `useStdIn` | `false` | Whether to pipe file contents via `stdin` as the input for the command. |
| `parallelise` | `false` | Whether to run the commands in this rule sequentially or in parallel. |
| `ignoreErrors` | `false` | Whether a rule should continue executing the command on targets if commands return non-zero exit codes. Does not apply to hooks. |
| `pre` | `[]` | A command to run before the rule is applied. The rule will not execute if this hook returns a non-zero exit code. |
| `command` | `[]` | A command to run for each target. Sequential commands, post hook and subrules will not execute if this command returns a non-zero exit code, unless `ignoreErrors` is true. |
| `post` | `[]` | A command to run after the rule is applied. Subrules will not execute if this hook returns a non-zero exit code. |
| `environment` | `{}` | A map of environment variables to set for the command. |
| `rules` | `[]` | A list of subrules. A rule is executed before its subrules. Rules are executed depth-first. |

### Environment variables available to commands
The following environment variables are available to commands:
| Variable | Description | Example |
| --- | --- | --- |
| `FILEPATH` | The full path of the target file. | `/home/user/project/src/file.txt` |
| `FILENAME` | The name of the target file. | `file.txt` |
| `FILEEXT` | The extension of the target file. | `txt` |
| `FILEDIR` | The directory of the target file. | `/home/user/project/src` |

---
## Setting up a local development environment

It is recommended to use [ghcup](https://www.haskell.org/ghcup/) to manage your Haskell environment. Once installed, use the following versions to ensure compatibility with the project:

| Tool | Version |
| --- | --- |
| GHC | 9.2.4 |
| HLS | 1.8.0 |
| Stack | 2.9.3 |
| Cabal | 3.6.2 |

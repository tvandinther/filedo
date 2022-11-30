# filedo

```
Filedo.config = Rule

Command = [String]

Rule = 
    priority :: Integer
    skip :: Boolean
    targets :: [Glob]
    exclude :: [Glob]
    pre :: Command
    command :: Command
    post :: Command
    environment :: {Key Value}
    Rules :: [Rule]
```

- `priority` is the priority of the rule. The higher the priority, the earlier the rule is applied.
- `skip` is a flag to skip the rule.
- `targets` is a list of glob patterns to match the target files. Patterns are scoped to the current target.
- `exclude` is a list of glob patterns to exclude the target files.
- `pre` is a command to run before the rule is applied.
- `command` is a command to run for each target.
- `post` is a command to run after the rule is applied.
- `environment` is a list of environment variables to set for the Rule.
- `Rules` is a list of subrules. A rule is executed before its subrules.

Defaults:
- `priority` = 0.
- `skip` = false.
- `targets` = `["**"]`.
- `exclude` = `[]`.
- `pre` = `[]`.
- `command` = `[]`.
- `post` = `[]`.
- `environment` = `{}`.
- `Rules` = `[]`.

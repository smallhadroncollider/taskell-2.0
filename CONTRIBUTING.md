## Workflow

Use the [Git Flow workflow](https://nvie.com/posts/a-successful-git-branching-model/)

## Hooks

Please use the following Git Hooks.

### `pre-commit`

```shell
#! /bin/zsh

if rg "\{-# OPTIONS_GHC|Prelude \(undefined\)" --glob='!test/Spec.hs' --glob='!CONTRIBUTING.md'; then
    exit 1
else
    stack test --work-dir .stack-test
fi
```

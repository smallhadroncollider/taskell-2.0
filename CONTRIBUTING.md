# Contributing

## Git

### Workflow

Use the [Git Flow workflow](https://nvie.com/posts/a-successful-git-branching-model/)

### Hooks

Please use the following Git Hooks.

#### `pre-commit`

```shell
#! /bin/zsh

if rg "\{-# OPTIONS_GHC|Prelude \(undefined\)" --glob='!test/Spec.hs' --glob='!CONTRIBUTING.md'; then
    exit 1
else
    stack test --work-dir .stack-test
fi
```

## Code

### Optics

- `a ^. x`: read the `x` property of `a`
- `a & x .~ y`: set the `x` property of `a` to `y`
- `a & x %~ fn`: update the `x` property of `a` using `fn`

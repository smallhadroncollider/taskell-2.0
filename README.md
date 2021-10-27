> I'm going to be taking a break from development of Taskell for a while. Trying to do it alongside my day job was proving a bit much. Feel free to continue to submit issues, but I won't be able to work on any of them immediately. Pull requests also welcome. Thanks for understanding 🙂

# Taskell 2.0

A complete rewrite of taskell focussing on application architecture and performance.

I wrote the original version of taskell when I was first learning Haskell. As a result I made many bad decisions that would be hard to re-engineer in place.

I'm taking a data first approach: creating the relevant data structures and parsers, then moving onto IO, and finally getting the UI in place once all of that is benchmarked and tested.

### Improvements

- More tests: many more tests than the original version. Taking a largely TDD approach.
- Benchmarking: most functionality is benchmarked
- Better data structure: the original data structure was just a three level sequence. This makes things like adding tags/contributors/related tasks difficult to do efficiently.
- Serialisation: using a `Builder` for much faster `ByteString` generation

## Benchmarking

### Setup

Install [direnv](https://direnv.net) and [criterion-cmp](https://hackage.haskell.org/package/criterion-cmp):

```shell
brew install direnv
cabal install criterion-cmp
```

In the project directory:

```shell
direnv allow
```

Adds all scripts in `.bin/` to your `$PATH` (in this working directory only).

### Running

All benchmarks:

```shell
bench
```

A specific group:

```shell
bench-group <group-name>
```

# toml-parse

Haskell library for reading TOML and easily querying it via either a string query or via lenses.

[![Build Status](https://travis-ci.org/pliosoft/toml-parse.svg?branch=master)](https://travis-ci.org/pliosoft/toml-parse)

## WARNING: This is still heavily-in-progress code

Pliosoft is in the process of becoming a Haskell-centric open source company, and so we are replacing internal libraries with
open source alternatives. Please feel free to contribute.

## Alternatives:

There are several existing TOML libraries for haskell already:

- [htoml][] parser library
- [toml][] package.

We came close to adopting htoml, but we found it important to have a good way to query parsed documents, and
it was quite difficult to retrofit that for us.

## toml-test

To run the [BurntSushi][] tests:

```console
git submodule update --init
```

To clone the test files to the `toml-test` directory. These will now be picked
up by the `BurntSushiSpec` file and run as part of `stack test`.

[BurntSushi]: https://github.com/BurntSushi/toml-test
[htoml]: https://github.com/cies/htoml
[toml]: https://hackage.haskell.org/package/toml

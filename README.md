# toml-parse
Haskell library for reading TOML and easily querying it. 

**This is still heavily-in-progress code**. 

We are replacing a ruby implementation, and decided to proceed with the implementation as open-source. 

## toml-test

To run the [BurntSushi][] tests:

1. Install `go`, export `$GOPATH`, and add `$GOPATH/bin` to `$PATH`
1. Install `toml-test`:

  ```
  go get github.com/BurntSushi/toml-test
  ```

1. Run a test:

  ```
  stack install && toml-test toml-decoder bool
  ```

TODO: make these executables conditional (e.g. `-ftoml-test`)

[BurntSushi]: https://github.com/BurntSushi/toml-test

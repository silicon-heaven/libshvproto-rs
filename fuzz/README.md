# How to run the fuzzers
## libfuzzer fuzz tests
### Install cargo-fuzz
```bash
cargo install cargo-fuzz
```
### Run the fuzz test
```
# libfuzzer_from_chainpack can be any of the libfuzzer tests
cargo fuzz run libfuzzer_from_chainpack
```
The fuzz test runs until a crash is found.

## AFL fuzz tests
### Install cargo-afl
```bash
cargo install cargo-afl
```
### Build the fuzz test
```
# afl_from_chainpack can be any of the AFL tests
cargo afl build --bin afl_from_chainpack
```

### Run the fuzz test
AFL fuzz testing requires example valid inputs. These live in the `src/afl-in-*` directories, and correspond to the tests.

```
# from the `fuzz/` directory
cargo afl fuzz -i ./src/afl-in-from-chainpack -o afl-out ../target/debug/afl_from_chainpack
```
The fuzz test runs continously, interrupt it to cancel. Output goes inside the `afl-out` directory.

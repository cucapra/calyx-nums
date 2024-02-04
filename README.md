# A Numerics Frontend for Calyx

## Installation

You'll need a recent version of [Rust][rust] (1.70+) and the [Sollya][sollya]
tool. On macOS, you can install the latter via [Homebrew][brew] with
`brew install sollya`. On Ubuntu or Debian, use `apt install sollya`.

Finally, clone the repository and build with `cargo build`.

### Integrating with `fud`

Optionally, you can register the compiler as an external stage for the
[Calyx driver][fud], `fud`. After installing `fud`, register the compiler with

    fud register -p <absolute path to calyx-nums repository>/fud/stage.py calyx-nums

[brew]: https://brew.sh/
[fud]: https://docs.calyxir.org/running-calyx/fud/index.html
[rust]: https://doc.rust-lang.org/cargo/getting-started/installation.html
[sollya]: https://www.sollya.org/

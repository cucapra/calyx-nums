# The FPCore Frontend for Calyx

Compile [FPCore][fpcore] to fixed-point hardware via [Calyx][calyx].

## Installation

You'll need [Rust][rust] (1.88+) and the [Sollya][sollya] tool. On macOS, you
can install the latter via [Homebrew][brew] with `brew install sollya`. On
Ubuntu or Debian, use `apt install sollya`.

Finally, clone the repository and build with `cargo build`.

### `fud2` Support

Optionally, you can register the frontend with the [Calyx driver][fud2], `fud2`.
To make the compiler accessible from `fud2`, add it to the plugins list in your
`fud2.toml` file:

```toml
plugins = ["<repository root>/tools/fud/fpcore.rhai"]

[calyx-libm]
base = "<repository root>"
```

[brew]: https://brew.sh/
[calyx]: https://calyxir.org/
[fpcore]: https://fpbench.org/spec/fpcore-2.0.html
[fud2]: https://docs.calyxir.org/running-calyx/fud2/index.html
[rust]: https://doc.rust-lang.org/cargo/getting-started/installation.html
[sollya]: https://www.sollya.org/

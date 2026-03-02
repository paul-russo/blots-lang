# Agents

## Cursor Cloud specific instructions

### Project overview

Blots is a small, functional, expression-oriented programming language written in Rust. The workspace has three crates (`blots-core`, `blots`, `blots-wasm`) plus an Astro documentation website in `website/`.

### Key commands

- **Build all**: `just build-all` (or `cargo build` + `just build-wasm`)
- **Test all**: `just test` (runs `cargo nextest run`, WASM tests, and Python unit-conversion validation)
- **Lint**: `cargo clippy --all-targets`
- **Run CLI**: pipe empty JSON or provide `-e` flag when running without a file to avoid stdin blocking, e.g. `echo '{}' | cargo run -p blots -- 'output x = 1 + 1'`
- **Website dev**: `cd website && bun run dev`

See `justfile` for the full task-runner command list.

### Gotchas

- The Blots CLI reads stdin for JSON input by default. When running non-interactively, always pipe input (even `echo '{}'`) or the process will hang waiting for EOF.
- `cargo-nextest` must be installed with `--locked` flag: `cargo install --locked cargo-nextest`.
- The `rust-toolchain.toml` pins Rust 1.89.0 with `profile = "minimal"`, so `clippy` must be added separately via `rustup component add clippy`.
- WASM tests require the `wasm32-unknown-unknown` target: `rustup target add wasm32-unknown-unknown`.

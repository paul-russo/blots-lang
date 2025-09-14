# Repository Guidelines

## Project Structure & Module Organization
- Workspace crates:
  - `blots-core/` – core language engine: parser, evaluator, built-ins (`ast.rs`, `parser.rs`, `expressions.rs`, `values.rs`).
  - `blots/` – CLI and REPL (`src/main.rs`, `cli.rs`, flags: `--evaluate/-e`, `--input/-i`, `--output`).
  - `blots-wasm/` – WebAssembly wrapper (exports via `wasm-bindgen`).
- `examples/` – sample Blots programs. `tests/` – language fixtures; `blots/tests/` – CLI integration tests.
- `Cargo.toml` (workspace) and `Makefile` define common tasks.

## Build, Test, and Development Commands
- `make build` – debug build for all crates (`cargo build`).
- `make release` – optimized build + `wasm-pack build --target bundler` in `blots-wasm/`.
- `make test` or `cargo test` – run unit and integration tests.
- `make wasm` – build only the WASM package.
- Run CLI locally:
  - `cargo run -p blots -- path/to/file.blot -i '{"x":1}'`
  - `echo "output result = 42" | cargo run -p blots -- -e`

## Coding Style & Naming Conventions
- Rust 2021, 4-space indent. Modules/files `snake_case`; types `PascalCase`; fns/vars `snake_case`.
- Prefer `anyhow::Result` and `?` for error propagation. Keep `pub` surface minimal in `blots-core`.
- Format and lint before pushing: `cargo fmt` and `cargo clippy --all-targets -- -D warnings`.

## Testing Guidelines
- Unit tests live beside code under `#[cfg(test)]` (see `blots-core/src/*`).
- CLI integration tests in `blots/tests/cli_flags.rs`.
- Add tests for new syntax, built-ins, or CLI flags. Example: extend tests in `blots-core/src/functions.rs` or add a new `#[test]` in `blots/tests/`.
- Run all tests with `cargo test`; keep outputs deterministic.

## Commit & Pull Request Guidelines
- Commits: concise, imperative mood, start with a capital; scope tags optional (e.g., "Add output flag", "Fix input handling").
- PRs: include a clear summary, rationale, and linked issues; highlight user-visible CLI changes; update tests and docs (`README.md`, `examples/`) accordingly.

## Security & Tooling
- Requires stable Rust and `wasm-pack` for WASM builds. Avoid `unsafe`—the interpreter is intended to stay in safe Rust.

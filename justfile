# Build only WASM
build-wasm:
    cd blots-wasm && wasm-pack build --target bundler

# Build only the CLI
build-cli:
    cargo build -p blots

# Build everything in debug mode
build-all:
    cargo build
    cd blots-wasm && wasm-pack build --target bundler

# Build everything in release mode, including WASM
build-all-release:
    cargo build --release
    cd blots-wasm && wasm-pack build --target bundler

# Publish a new version to Crates.io, and add a release on GitHub with pre-built binaries.
release version:
    ./release.sh {{version}}

# Run tests
test:
    cargo nextest run
    cd blots-wasm && wasm-pack test --node
    UV_CACHE_DIR=.uv-cache uv run --python 3.11 --with pint python scripts/check_unit_conversions.py

# Run tests with CI profile (retries, fail-fast)
test-ci:
    cargo nextest run --profile ci
    cd blots-wasm && wasm-pack test --node
    UV_CACHE_DIR=.uv-cache uv run --python 3.11 --with pint python scripts/check_unit_conversions.py

# Run WASM tests only
test-wasm:
    cd blots-wasm && wasm-pack test --node

# Clean build artifacts
clean:
    cargo clean
    rm -rf blots-wasm/pkg

# Default recipe (runs when just `just` is called)
default: build-all

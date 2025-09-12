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

# Run tests
test:
    cargo test

# Clean build artifacts
clean:
    cargo clean
    rm -rf blots-wasm/pkg

# Default recipe (runs when just `just` is called)
default: build-all

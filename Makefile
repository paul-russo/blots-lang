.PHONY: build release test wasm clean

# Build everything in debug mode
build:
	cargo build

# Build everything in release mode, including WASM
release:
	cargo build --release
	cd blots-wasm && wasm-pack build --target bundler

# Run tests
test:
	cargo test

# Build only WASM
wasm:
	cd blots-wasm && wasm-pack build --target bundler

# Clean build artifacts
clean:
	cargo clean
	rm -rf blots-wasm/pkg
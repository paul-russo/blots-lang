# Workspace Management with cargo-workspaces

This repository uses [cargo-workspaces](https://github.com/pksunkara/cargo-workspaces) for managing the multi-crate workspace.

## Installation

```bash
cargo install cargo-workspaces
```

## Common Commands

### List all workspace crates
```bash
cargo ws list
```

### Check which crates have changed
```bash
cargo ws changed
```

### Version all crates (bumps version across workspace)
```bash
# Patch version bump (0.4.3 -> 0.4.4)
cargo ws version patch

# Minor version bump (0.4.3 -> 0.5.0)  
cargo ws version minor

# Major version bump (0.4.3 -> 1.0.0)
cargo ws version major

# Custom version
cargo ws version custom 0.5.0-beta.1
```

### Publish crates to crates.io
```bash
# Publish with version bump
cargo ws publish patch

# Dry-run to see what would be published
cargo ws publish --dry-run

# Publish without version bump
cargo ws publish --publish-as-is
```

### Execute commands in all crates
```bash
# Run tests in all crates
cargo ws exec cargo test

# Build all crates
cargo ws exec cargo build

# Format all crates
cargo ws exec cargo fmt
```

## Workspace Structure

The workspace consists of three crates:
- `blots-core`: Core language implementation library
- `blots`: CLI interpreter
- `blots-wasm`: WebAssembly bindings

All crates share a common version managed at the workspace level in the root `Cargo.toml`.

## Release Process

The project uses cargo-dist for creating releases. When you're ready to release:

1. Use cargo-workspaces to bump the version:
   ```bash
   cargo ws version patch
   ```

2. This will:
   - Update all crate versions
   - Create a git commit with message "Release v{version}"
   - Create a git tag "v{version}"

3. Push the changes and tag:
   ```bash
   git push origin main --tags
   ```

4. The GitHub Actions workflow will automatically create release artifacts using cargo-dist.

## Configuration

The workspace is configured in the root `Cargo.toml`:
- `[workspace.metadata.workspaces]` section contains cargo-workspaces specific configuration
- `allow_branch` is set to `["main"]` to only allow releases from the main branch
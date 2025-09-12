# Publishing to crates.io

This workspace is configured to publish all crates to crates.io with synchronized versions.

## Overview

The workspace uses a hybrid dependency approach:
- **Development**: Uses path dependencies (`blots-core = { path = "../blots-core" }`)
- **Publishing**: Temporarily switches to version dependencies (`blots-core = { version = "X.Y.Z" }`)

This allows you to:
1. Develop locally with immediate access to changes across crates
2. Publish all crates together with the same version number
3. Avoid manual version synchronization between crates

## Publishing Process

### 1. Regular Release (Git tag only)
```bash
./release.sh 1.0.0
```

This will:
- Update the workspace version to `1.0.0` in `Cargo.toml`
- Update the `blots-core` dependency version to `1.0.0`
- Commit changes with message "Release v1.0.0"
- Create and push git tag `v1.0.0`

### 2. Release + Publish to crates.io
```bash
./release.sh 1.0.0 --publish
```

This will do everything above, plus:
- Publish `blots-core` to crates.io first
- Temporarily switch workspace to use the published `blots-core` version
- Publish `blots` and `blots-wasm` to crates.io
- Restore the original workspace configuration (path dependencies)
- **All-or-nothing guarantee**: If any crate fails to publish, all successfully published crates will be automatically yanked to maintain consistency

## Prerequisites for Publishing

1. **Cargo login**: Ensure you're authenticated with crates.io:
   ```bash
   cargo login
   ```

2. **Clean working directory**: All changes must be committed before running the release script.

3. **Main branch**: Releases can only be made from the `main` branch.

## Workspace Configuration

The workspace uses inheritance for common package metadata:

```toml
[workspace.package]
version = "1.0.0"                                    # Shared version
authors = ["Paul Russo <paul@paulrusso.me>"]
edition = "2024"
repository = "https://github.com/paul-russo/blots-lang"
homepage = "https://github.com/paul-russo/blots-lang"
license-file = "LICENSE"

[workspace.dependencies]
# Internal dependency - uses both path (dev) and version (publish)
blots-core = { path = "blots-core", version = "1.0.0" }
```

Each crate inherits these values:

```toml
[package]
name = "blots"
description = "A small, simple, expression-oriented programming language."
# Inherit from workspace
version.workspace = true
edition.workspace = true
authors.workspace = true
# ...

[dependencies]
blots-core.workspace = true  # Uses workspace dependency
```

## Dependency Order

Crates are published in dependency order:
1. `blots-core` (no internal dependencies)
2. `blots` (depends on `blots-core`)
3. `blots-wasm` (depends on `blots-core`)

## Troubleshooting

### All-or-Nothing Publishing

The release script implements an all-or-nothing publishing strategy:

- **Success**: All crates are published together
- **Failure**: If any crate fails to publish, all previously published crates in the batch are automatically yanked
- **Recovery**: Simply fix the issue and re-run `./release.sh VERSION --publish`

This ensures your workspace crates are always consistent on crates.io - either all available together or none at all.

### "no matching package named blots-core found"

This error occurs when trying to package crates that depend on `blots-core` before it's published to crates.io. This is expected behavior and is resolved by the release script's dependency switching.

### License file warnings

You may see warnings like:
```
license-file `../LICENSE` appears to be a path outside of the package
```

This is harmless. Each crate has its own `LICENSE` file that will be included in the published package.

### Publishing individual crates

If you need to publish individual crates manually:

1. For `blots-core`:
   ```bash
   cd blots-core && cargo publish
   ```

2. For other crates, first ensure `blots-core` is published, then temporarily switch the workspace dependency:
   ```bash
   # Edit Cargo.toml to change:
   # blots-core = { path = "blots-core", version = "X.Y.Z" }
   # to:
   # blots-core = { version = "X.Y.Z" }
   
   cd blots && cargo publish
   cd blots-wasm && cargo publish
   
   # Don't forget to restore the path dependency afterward!
   ```

The release script automates this process for you.

# Blots Test Suite Documentation

## Overview

This document describes the test suite for the Blots programming language, including unit tests, integration tests, and WASM-specific tests.

## Test Categories

### 1. Source Map Unit Tests (`blots-core/src/source_map.rs`)

Tests for the source mapping infrastructure that enables mapping between Blots source code and generated JavaScript:

- `test_source_map_builder_tracks_position` - Verifies position tracking as code is generated
- `test_source_map_builder_with_newlines` - Tests handling of newlines in generated code
- `test_source_map_add_mapping` - Tests adding source-to-generated mappings
- `test_source_map_to_comment` - Verifies source map serialization to base64 comment
- `test_map_js_position_to_source` - Tests mapping JS positions back to Blots source
- `test_source_map_builder_with_mappings` - Tests complete source map building workflow

### 2. WASM Integration Tests (`blots-wasm/src/tests.rs`)

Tests for the WebAssembly module's JavaScript API:

#### Transpilation Tests
- `test_transpile_without_source_map` - Verifies transpile returns string when sourceMapped=false
- `test_transpile_with_source_map` - Verifies transpile returns object with code and sourceMap when sourceMapped=true
- `test_transpile_with_inline_eval_without_source_map` - Tests inline eval mode without source maps
- `test_transpile_with_inline_eval_with_source_map` - Tests inline eval mode with source maps

#### Evaluation Tests
- `test_evaluate_without_source_map` - Tests evaluate function works normally without source maps
- `test_evaluate_with_source_map_no_error` - Tests evaluate with source maps for valid code
- `test_evaluate_with_source_map_type_error` - Verifies errors are caught with source mapping enabled
- `test_evaluate_without_source_map_type_error` - Verifies errors are caught without source mapping

#### Utility Function Tests
- `test_get_built_in_function_names` - Tests built-in function listing
- `test_get_constants` - Tests constant retrieval
- `test_tokenize` - Tests tokenization functionality

### 3. Error Enhancement Tests (`blots-wasm/src/error_tests.rs`)

Tests for enhanced error message functionality that provides better debugging information:

- `test_enhance_error_message_add_operation` - Tests error enhancement for addition type errors
- `test_enhance_error_message_multiply_operation` - Tests error enhancement for multiplication type errors
- `test_enhance_error_message_divide_operation` - Tests error enhancement for division type errors
- `test_enhance_error_message_multiline_with_comments` - Tests handling of multi-line code with comments
- `test_enhance_error_message_no_match` - Tests fallback behavior when no pattern matches

## Running the Tests

### Standard Rust Tests

Run all tests for a specific package:
```bash
# Run all blots-core tests
cargo test -p blots-core

# Run all blots-wasm tests (excluding WASM-specific tests)
cargo test -p blots-wasm
```

Run specific test modules:
```bash
# Run source map tests
cargo test --lib -p blots-core source_map::tests

# Run error enhancement tests
cargo test --lib -p blots-wasm error_tests::
```

### WASM-Specific Tests

WASM integration tests require `wasm-pack` and must be run in a browser environment:

```bash
# Install wasm-pack if not already installed
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh

# Run WASM tests in Firefox (from project root)
wasm-pack test --headless --firefox
```

**Note**: 
- WASM tests cannot be run with standard `cargo test` due to JsError not implementing the Debug trait, which is a limitation of wasm-bindgen.
- The tests are configured to run in a browser environment using `wasm_bindgen_test_configure!(run_in_browser)`.
- If you encounter module type errors, ensure the parent directory's package.json doesn't conflict with the test runner's requirements.

## Test Coverage Areas

The test suite ensures correctness in the following areas:

- **Source Mapping Infrastructure**
  - Source map data structure creation and manipulation
  - Position tracking during code generation
  - Mapping JavaScript errors back to Blots source positions

- **API Functionality**
  - Transpilation with and without source maps
  - Code evaluation with proper error handling
  - Inline evaluation mode
  - Built-in function and constant access

- **Error Handling**
  - Enhanced error messages with source location information
  - Type error detection and reporting
  - Graceful fallback for unrecognized error patterns

- **Backward Compatibility**
  - All functions maintain compatibility when source mapping is disabled
  - Optional parameters default to previous behavior

## Adding New Tests

When adding new functionality to Blots:

1. **Unit Tests**: Add tests in the appropriate module's test submodule
2. **Integration Tests**: Add tests to the relevant test file in the package
3. **WASM Tests**: Use `#[wasm_bindgen_test]` attribute for browser-based tests
4. **Error Cases**: Include tests for both success and failure scenarios

## Continuous Integration

Tests should be run as part of the CI pipeline:
```yaml
# Example CI configuration
- cargo test --all
- cd blots-wasm && wasm-pack test --headless --firefox
```
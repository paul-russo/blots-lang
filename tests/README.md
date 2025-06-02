# Blots Test Files

This directory contains test files for the Blots programming language and its JavaScript transpiler.

## Directory Structure

### `transpiler/`
Tests specifically for the JavaScript transpiler functionality:

- **`builtin-test.blot/.js`** - Tests built-in functions (map, filter, math functions, etc.)
- **`complex-test.blot`** - Tests complex math operations and timing functions
- **`debug-access.blot`** - Tests array access transpilation (was used to debug `arr[0]` issues)
- **`inline-test.blot/.js`** - Tests inline evaluation feature for Soulver-like behavior
- **`output-test.blot`** - Tests output declarations and print statements
- **`simple-debug.blot`** - Simple array access test case
- **`standalone-test.blot/.js`** - Tests standalone expressions (not bound to variables)
- **`test.blot`** - Basic transpiler functionality test with various language features

### `language-features/`
Tests for specific Blots language features:

- **`benchmark-pattern.blot`** - Tests the benchmark pattern with factorial and complex operations
- **`complex-each-with.blot`** - Tests complex each/with combinations for functional programming
- **`each-with-test.blot`** - Tests each() function and with operator for mapping

## Running Tests

### Transpiler Tests
```bash
# Basic transpilation
cargo run -p blots -- tests/transpiler/test.blot --transpile

# With inline evaluation (Soulver-like results)
cargo run -p blots -- tests/transpiler/inline-test.blot --transpile --inline-eval

# Run transpiled JavaScript
cargo run -p blots -- tests/transpiler/builtin-test.blot --transpile | node
```

### Language Feature Tests  
```bash
# Test functional programming patterns
cargo run -p blots -- tests/language-features/each-with-test.blot --transpile | node

# Test complex benchmarks
cargo run -p blots -- tests/language-features/benchmark-pattern.blot --transpile | node
```

## Test Categories

1. **Built-in Functions**: Testing map, filter, collect, math operations, string functions
2. **Inline Evaluation**: Testing position-based result capture for editor integration
3. **Language Features**: Testing each/with operators, functional programming patterns
4. **Edge Cases**: Testing array access, object literals in lambdas, naming conflicts
5. **Integration**: Testing complex real-world usage patterns like benchmarks
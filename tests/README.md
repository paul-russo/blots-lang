# Blots Test Files

This directory contains test files for the Blots programming language.

## Directory Structure

### `language-features/`
Tests for specific Blots language features:

- **`benchmark-pattern.blots`** - Tests the benchmark pattern with factorial and complex operations
- **`complex-each-with.blots`** - Tests complex each/with combinations for functional programming
- **`each-with-test.blots`** - Tests each() function and with operator for mapping

## Running Tests

### Language Feature Tests  
```bash
# Test functional programming patterns
cargo run -p blots -- tests/language-features/each-with-test.blots
```

## Test Categories

1. **Built-in Functions**: Testing map, filter, collect, math operations, string functions
2. **Inline Evaluation**: Testing position-based result capture for editor integration
3. **Language Features**: Testing each/with operators, functional programming patterns
4. **Edge Cases**: Testing array access, object literals in lambdas, naming conflicts
5. **Integration**: Testing complex real-world usage patterns like benchmarks
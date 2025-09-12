# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Structure

Blots is a functional programming language implemented in Rust with three main components:

- **blots-core**: Core language implementation with parser, evaluator, and standard library
- **blots**: CLI interpreter for running Blots programs 
- **blots-wasm**: WebAssembly bindings for web integration

## Development Commands

### Building
```bash
cargo build                    # Build all workspace members
cargo build -p blots          # Build CLI only
cargo build -p blots-wasm     # Build WASM bindings
```

### Running
```bash
cargo run -p blots                      # Start interactive REPL
cargo run -p blots <file.blot>          # Execute a Blots program file
cargo run -p blots "2 + 3"               # Execute inline Blots code
echo "output x = 42" | cargo run -p blots -e  # Execute piped Blots code
```

### Testing
```bash
cargo test                     # Run all tests
cargo test -p blots-core      # Test core library only
```

### WASM Development
```bash
cd blots-wasm
wasm-pack build --target bundler   # Build WASM package for bundler
```

## Language Architecture

### Parser (blots-core/src/parser.rs)
Uses Pest parser generator with grammar defined in `grammar.pest`. The grammar supports:
- Functional programming constructs (lambdas, higher-order functions)
- Infix and postfix operators with precedence
- Collections (lists, records) with spread syntax
- Conditionals and pattern matching
- Assignment and output declarations

### Evaluator (blots-core/src/expressions.rs)
Expression-based evaluator with:
- Immutable values and reference-counted heap
- Lexical scoping with nested environments
- Tail call optimization for recursion
- Built-in function library

### Value System (blots-core/src/values.rs)
Supports numbers, booleans, strings, lists, records, and functions. Values are heap-allocated with reference counting for memory management.

### Standard Library (blots-core/src/functions.rs)
Extensive built-in functions for mathematical operations, list processing, string manipulation, and I/O operations.

## CLI Features

The CLI (`blots/src/main.rs`) supports:
- Interactive REPL with expression evaluation and syntax highlighting
- File execution mode
- Inline code execution (pass Blots code directly as argument)
- Piped code execution with `-e` flag
- JSON input/output handling via `-i` and `-o` flags
- Simple command system (quit, help)

## WASM Integration

The WASM module (`blots-wasm/src/lib.rs`) exposes:
- `evaluate()`: Execute Blots code with input parameters
- `tokenize()`: Parse and tokenize source code
- `get_built_in_function_names()`: List available built-in functions
- `get_constants()`: Access language constants

## Testing

The `tests/` directory contains test files for various language features, including compound interest calculations, comments, and blank line handling. A `tests/README.md` file provides additional test documentation.

## File Extensions

Blots source files use the `.blot` extension. See `examples/` directory for sample programs demonstrating language features.

## Formatting

Before finishing a task, or committing code, make sure to run `cargo fmt`.

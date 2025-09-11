# Blots

Blots is a small, dynamic, functional, expression-oriented programming language designed to be quick to learn, easy to use, and to produce code that's readable yet reasonably compact. Blots is intended for quick calculations and data transformation. The Blots interpreter is written in safe Rust.

## Installation

### Compiling From Source

Once you've cloned this repo, the Blots CLI can be compiled from source and installed in your PATH with a single command:
```
cargo install --path blots
```

### Installing a Prebuilt Binary

Coming soon.

## The Blots Language

### Core Types

- **Number**: 64-bit float with decimal/sci-notation support and `_` separators (e.g., `1_000_000`, `3.14e-2`)
- **String**: Single or double quotes (`'hello'`, `"world"`); concatenate with `+`
- **Boolean**: `true`, `false`; operators: `and`/`&&`, `or`/`||`, `not`/`!`
- **Null**: `null`
- **List**: Ordered collection `[1, 2, 3]`; access with `list[index]` (0-based); spread with `[...list1, ...list2]`
- **Record**: Key-value pairs `{a: 1, "hello there": "hi"}`; key shorthand `{foo}`; access with `record.a` or `record["key"]`
- **Function**: Arrow functions `x => x+1`, `(x,y?) => x + (y ?? 0)`, `(f, ...rest) => map(rest,f)`

### Operators & Control Flow

- **Arithmetic**: `+ - * / % ^ !`
- **Comparison**: `== != < <= > >=`
- **Logic**: `&& || !` or `and or not`
- **Special**: `??` (null-coalesce), `...` (spread)
- **Conditional**: `if cond then expr else expr`

### Built-in Functions

#### Math
`sqrt sin cos log exp abs floor ceil round`

#### Lists
`len head tail slice map filter reduce sort`

#### Statistics
`min max avg median sum prod percentile`

#### Strings
`split join replace uppercase lowercase`

#### Records
`entries keys values`

#### Utility
- `range(n)` - returns `[0, 1, ..., n-1]`
- `range(start, end)` - returns `[start, start+1, ..., end-1]`
- `typeof` - returns the type of a value (either "number", "string", "boolean", "null", "list", "record", "built-in function", or "function")

### Constants

Access mathematical constants via `constants.*`:
- `constants.pi`: The mathematical constant _π_.
- `constants.e`: The mathematical constant _e_.
- `constants.max_value`: The maximum value that can be represented as a 64-bit floating point number.
- `constants.min_value`: The minimum non-zero value that can be represented as a 64-bit floating point number.

### Bindings

There are no mutable variables in Blots. Instead, values are _bound_ to a _name_. Once created, a _binding_ cannot be mutated; it's the same for the life of the program. This property makes Blots code more "pure": it is difficult to construct an expression in Blots that can return a different result for the same inputs. This also means that functions can be losslessly serialized and `output` from programs.

### Broadcasting

Arithmetic operations automatically broadcast over lists:

```blots
[1, 2, 3] * 10  // [10, 20, 30]
```

Pipes (`|>`) are also broadcast over lists:

```blots
'hello' |> uppercase // 'HELLO'
['hello', 'world'] |> uppercase // ['HELLO', 'WORLD']
```

### `do` Blocks

Blots is an expression-oriented language, in the sense that every "thing" in a Blots program should evaluate to a useful value. Sometimes it's more intuitive to represent a computation as a series of steps instead of composing functions. For these cases, you can use `do` blocks for imperative code with intermediate variables:
```blots
result = do {
  y = x * 2
  z = -y
  return z          
}
```

Some things to note about `do` blocks:
- Since each `do` block is an expression, it must end with a `return` statement so that the entire block evaluates to a useful value.
- Statements in `do` blocks can be separated by newlines. Alternatively, if you're writing a single-line block, you can use semicolons (`;`) to separate statements.

### Inputs and Outputs

#### Inputs

The Blots CLI accepts JSON values as inputs, either as piped input or via the `--input` (`-i`) flag:
```
> blots -i '{ "name": "Paul" }'
```

All input values are merged together and made available via the `inputs` record:
```blots
output greeting = "Hey " + inputs.name // "Hey Paul"
```

JSON arrays and primitive values (numbers, strings, booleans, and `null`) can be passed directly as inputs as well:
```
> blots -i '[1,2,3]'
```

These unnamed inputs are named like `value_{1-based index}`:
```blots
output total = sum(...inputs.value_1) // 6
```

#### Outputs

Use the `output` keyword to include bound values in the `outputs` record. This record will be sent to stdout as a JSON object when your Blots program successfully executes (or when you close an interactive Blots session). The `output` keyword can be used in two ways:

```blots
// For new bindings
output one = 1

// For existing bindings
answer = 42
output answer
```

The above example would yield this output:

```json
{ "one": 1, "answer": 42 }
```

### Comments

Comments start with `//`, and run until the end of the line:

```blots
// This is a comment
x = 42 // This is also a comment
```

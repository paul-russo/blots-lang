# Blots

Blots is a small, dynamic, functional, expression-oriented programming language designed to be quick to learn, easy to use, and to produce code that's readable yet reasonably compact. Blots is intended for quick calculations and data transformation.

## Installation

### Installing a Prebuilt Binary

#### Homebrew

`brew install paul-russo/tap/blots`

### Building from Source, Using `cargo`

> If you don't have Rust installed, you can use [rustup](https://rustup.rs/) to install the latest stable version of Rust, including the `cargo` tool.

```
cargo install blots
```

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

### Bindings

There are no mutable variables in Blots. Instead, values are _bound_ to a _name_. Once created, a _binding_ cannot be mutated; it's the same for the life of the program. This property makes Blots code more "pure": it is difficult to construct an expression in Blots that can return a different result for the same inputs. This also means that functions can be losslessly serialized and `output` from programs.

### Broadcasting

Arithmetic and comparison operations automatically "broadcast" over lists, meaning they apply to each element:

```blots
[1, 2, 3] * 10  // [10, 20, 30] (because [1 * 10 = 10, 2 * 10 = 20, 3 * 10 = 30])
[4, 5, 6] > 3 // true (because [4 > 3 = true, 5 > 3 = true, 6 > 3 = true], so the condition is true for all elements)
```

### `via` and `into`

The `via` operator takes a value and sends it through a function, applying the function to each element if the value is a list. For example:

```blots
'hello' via uppercase // 'HELLO' (because uppercase('hello') = 'HELLO')
['hello', 'world'] via uppercase // ['HELLO', 'WORLD'] (because [uppercase('hello') = 'HELLO', uppercase('world') = 'WORLD'])
```

`into` works exactly the same as `via`, except there is no broadcasting. This means that you can "reduce" a list into a single value (though you could also produce another list). Example:

```blots
'hello' into head // 'h' (because head('hello') = 'h')
['hello', 'world'] via head // ['h', 'w'] (because [head('hello') = 'h', head('world') = 'w'])
['hello', 'world'] into head // 'hello' (because head(['hello', 'world']) = 'hello')
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
```bash
blots -i '{ "name": "Paul" }'
```

All input values are merged together and made available via the `inputs` record:
```blots
output greeting = "Hey " + inputs.name // "Hey Paul"
```

JSON arrays and primitive values (numbers, strings, booleans, and `null`) can be passed directly as inputs as well:
```bash
blots -i '[1,2,3]'
```

These unnamed inputs are named like `value_{1-based index}`:
```blots
output total = sum(...inputs.value_1) // 6
```

##### More Input Examples

**Multiple inputs:**
```bash
# Combine multiple JSON inputs
blots -i '{"x": 10}' -i '{"y": 20}' "output sum = inputs.x + inputs.y"
# Output: {"sum": 30}
```

**Piped input:**
```bash
# Pipe JSON data into Blots
echo '{"items": [1,2,3,4,5]}' | blots -e "output avg = avg(...inputs.items)"
# Output: {"avg": 3}

# Process command output
curl -s "https://api.example.com/data.json" | blots -e "output count = len(inputs.results)"
# Output: { "count": 20 }
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

##### More Output Examples

**Multiple outputs:**
```blots
// Calculate statistics from input data
data = inputs.values
output mean = avg(...data)
output min_val = min(...data)
output max_val = max(...data)
output std_dev = sqrt(avg(...map(data, x => (x - mean)^2)))
```

**Structured outputs:**
```blots
// Return nested data structures
output result = {
  summary: {
    total: sum(...inputs.items),
    count: len(inputs.items)
  },
  processed: map(inputs.items, x => x * 2)
}
```

**Using outputs with other tools:**
```bash
# Format output with jq
blots -i '[1,2,3,4,5]' "output stats = {min: min(...inputs.value_1), max: max(...inputs.value_1)}" | jq

# Save output to file
blots "output data = range(1, 11) via (x => x^2)" > squares.json

# Chain Blots programs
blots "output nums = range(1, 6)" | blots -e "output squares = inputs.nums via (x => x^2)"
```

### Comments

Comments start with `//`, and run until the end of the line:

```blots
// This is a comment
x = 42 // This is also a comment
```

### Built-in Functions

#### Math Functions
- `sqrt(x)` - returns the square root of x
- `sin(x)` - returns the sine of x (in radians)
- `cos(x)` - returns the cosine of x (in radians)
- `tan(x)` - returns the tangent of x (in radians)
- `asin(x)` - returns the arcsine of x (in radians)
- `acos(x)` - returns the arccosine of x (in radians)
- `atan(x)` - returns the arctangent of x (in radians)
- `log(x)` - returns the natural logarithm of x
- `log10(x)` - returns the base-10 logarithm of x
- `exp(x)` - returns _e_ raised to the power of x
- `abs(x)` - returns the absolute value of x
- `floor(x)` - returns the largest integer less than or equal to x (e.g. `2.7` becomes `2` and `-2.7` becomes `3`)
- `ceil(x)` - returns the smallest integer greater than or equal to x (e.g. `2.1` becomes `3`)
- `round(x)` - returns x rounded to the nearest integer (e.g. `2.7` becomes `3`)
- `trunc(x)` - returns the integer part of x (removes fractional part) (e.g. `2.7` becomes `2` and `-2.7` becomes `-2`)

#### Aggregate Functions
- `min(list)` - returns the minimum given value from a list
- `max(list)` - returns the maximum value from a list
- `avg(list)` - returns the average (mean) of values in a list
- `sum(list)` - returns the sum of all values in a list
- `prod(list)` - returns the product of all values in a list
- `median(list)` - returns the median value from a list
- `percentile(list, p)` - returns the percentile value at position p (0-100) from a list

#### List Functions
- `range(n)` - returns `[0, 1, ..., n-1]`
- `range(start, end)` - returns `[start, start+1, ..., end-1]`
- `len(list)` - returns the length of a list
- `head(list)` - returns the first element of a list
- `tail(list)` - returns all but the first element of a list
- `slice(list, start, end)` - returns a sublist from start (inclusive) to end (exclusive) indices
- `concat(list1, list2, ...)` - concatenates multiple lists
- `dot(list1, list2)` - returns the dot product of two lists
- `unique(list)` - returns unique elements from a list
- `sort(list)` - returns a sorted copy of the list (ascending)
- `sort_by(list, fn)` - sorts a list using a comparison function
- `reverse(list)` - returns a reversed copy of the list

#### Higher-Order Functions
- `map(list, fn)` - applies a function to each element of a list
- `reduce(list, fn, initial)` - reduces a list to a single value using a function
- `filter(list, fn)` - returns elements where the function returns true
- `every(list, fn)` - returns true if all elements satisfy the predicate
- `some(list, fn)` - returns true if any element satisfies the predicate

#### String Functions
- `split(string, delimiter)` - splits a string into a list
- `join(list, delimiter)` - joins a list into a string
- `replace(string, search, replacement)` - replaces occurrences in a string
- `trim(string)` - removes leading and trailing whitespace
- `uppercase(string)` - converts string to uppercase
- `lowercase(string)` - converts string to lowercase
- `to_string(value)` - converts a value to its string representation
- `to_number(string)` - converts a string to a number
- `includes(string, substring)` - checks if string contains substring
- `format(string, ...values)` - formats a string with placeholder values (e.g. `format("answer: {}", 42))

#### Type Functions
- `typeof(value)` - returns the type of a value ("number", "string", "boolean", "null", "list", "record", "built-in function", or "function")
- `arity(fn)` - returns the minimum number of parameters a function expects

#### Record Functions
- `keys(record)` - returns a list of all keys in a record
- `values(record)` - returns a list of all values in a record
- `entries(record)` - returns a list of [key, value] pairs from a record

### Constants

Access mathematical constants via `constants.*`:
- `constants.pi`: The mathematical constant _π_.
- `constants.e`: The mathematical constant _e_.
- `constants.max_value`: The maximum value that can be represented as a 64-bit floating point number.
- `constants.min_value`: The minimum non-zero value that can be represented as a 64-bit floating point number.

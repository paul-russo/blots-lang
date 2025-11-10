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
- **List**: Ordered collection `[1, 2, 3]`; access with `list[index]` (0-based); spread with `[...list1, ...list2]`
- **Record**: JSON compatible. Key-value pairs `{a: 1, "hello there": "hi"}`; key shorthand `{foo}`; access with `record.a` or `record["key"]`
- **Function**: `x => x+1`, `(x,y?) => x + (y ?? 0)`, `(f, ...rest) => map(rest,f)`
- **Null**: `null`

### Operators & Control Flow

- **Arithmetic**: `+ - * / % ^ !`
- **Comparison**: `== != < <= > >=` (with broadcasting)
- **Non-broadcasting comparison**: `.== .!= .< .<= .> .>=` (use these to compare entire lists as whole values)
- **Logic**: `&& || !` or `and or not`
- **Special**: `??` (null-coalesce), `...` (spread)
- **Conditional**: `if cond then expr else expr`

### Bindings

There are no mutable variables in Blots. Instead, values are _bound_ to a _name_. Once created, a _binding_ cannot be mutated; it's the same for the life of the program. This property makes Blots code more "pure": it is difficult to construct an expression in Blots that can return a different result for the same inputs. This also means that functions can be losslessly serialized and `output` from programs.

### Broadcasting

Arithmetic and comparison operations automatically "broadcast" over lists, meaning they apply to each element:

```blots
[1, 2, 3] * 10     // [10, 20, 30]
[10, 20, 30] + 2   // [12, 22, 32]
[4, 5, 6] > 3      // [true, true, true]
[1, 2] == [2, 2]   // [false, true]
```

#### Dot-Prefixed Comparison Operators

Sometimes you want to compare whole values without broadcasting. The dot-prefixed comparison operators (`.==`, `.!=`, `.<`, `.<=`, `.>`, `.>=`) disable broadcasting and perform direct value comparisons:

```blots
// Regular == with broadcasting
[10, 5, 10] == 10 // [true, false, true] (equality is evaluated for each element)

// Dot operator without broadcasting
[10, 5, 10] .== 10           // false (list isn't the same type as `10`)
[10, 5, 10] .== [10, 5, 10]  // true (lists are identical)
```

##### List Comparison Algorithm

For ordering operators (`.<`, `.<=`, `.>`, `.>=`), lists are compared **lexicographically** (like dictionary ordering):

1. **Element-by-element comparison**: Lists are compared element by element from left to right
2. **First difference decides**: The first non-equal pair of elements determines the result
3. **Length as tiebreaker**: If all compared elements are equal, the shorter list is considered less than the longer list

```blots
// Element-by-element comparison
[1, 2, 3] .< [1, 2, 4]   // true  (first two elements equal, 3 < 4)
[1, 2, 3] .< [1, 3, 0]   // true  (first element equal, 2 < 3)
[2, 0, 0] .> [1, 9, 9]   // true  (first element decides: 2 > 1)

// Length comparison when elements are equal
[1, 2] .< [1, 2, 3]      // true  (all common elements equal, shorter is less)
[] .< [1]                // true  (empty list is less than any non-empty list)
[1, 2, 3] .== [1, 2]     // false (different lengths)

// Nested lists work recursively
[[1, 2], [3]] .< [[1, 2], [3, 4]]  // true  ([3] < [3, 4])
[[2]] .> [[1, 9]]        // true  ([2] > [1, 9] because 2 > 1)
```

##### Equality Comparisons

For `.==` and `.!=`, lists must be exactly equal in both structure and values:

```blots
// Deep equality check
[1, 2, 3] .== [1, 2, 3]              // true  (same values, same order)
[[1, 2], [3, 4]] .== [[1, 2], [3, 4]]  // true  (nested equality)

// Any difference makes them unequal
[1, 2, 3] .!= [1, 2, 4]              // true  (different values)
[1, 2, 3] .!= [1, 2]                 // true  (different lengths)
[1, 2, 3] .!= 123                    // true  (different types)
```

##### Mixed Type Comparisons

When comparing different types with dot operators:
- `.==` and `.!=` always return `false` and `true` respectively for different types
- Ordering operators (`.<`, etc.) return `false` when types can't be ordered

```blots
"hello" .== [1, 2, 3]    // false (string != list)
5 .< [1, 2, 3]           // false (number and list have no natural ordering)
"abc" .< "def"           // true  (strings compare lexicographically)
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

Blots is an expression-oriented language, in the sense that every statement in a Blots program should evaluate to a useful value. This works well with a functional approach, where you compose functions to compute values. However, sometimes it's more intuitive to represent a computation as a series of discrete steps that happen one after another, instead of composing functions. For these cases, you can use `do` blocks to create an expression whose final value is the result of imperative code with intermediate variables:

```blots
result = do {
  y = x * 2
  z = -y
  return z          
}
```

Some things to note about `do` blocks:
- Since each `do` block is an expression and needs to evaluate to a single value, it must end with a `return` statement.
- Statements in `do` blocks are separated by newlines, just like other statements in Blots. Alternatively, if you want to keep things more compact, you can use semicolons (`;`) to separate statements on the same line.

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

##### Input Shorthand Syntax

For convenience, you can use the `#` character as shorthand for `inputs.`:
```blots
// These are equivalent:
output greeting = "Hey " + #name
output greeting = "Hey " + inputs.name

// Useful with the coalesce operator for default values:
principal = #principal ?? 1000
years = #years ?? 10
```

The `#field` syntax works everywhere `inputs.field` works and returns `null` for missing fields (making it compatible with the `??` operator).

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
blots -i '{"x": 10}' -i '{"y": 20}' "output total = inputs.x + inputs.y"
# Output: {"total": 30}
```

**Piped input:**
```bash
# Pipe JSON data into Blots
echo '{"items": [1,2,3,4,5]}' | blots -e "output average = avg(...inputs.items)"
# Output: {"average": 3}

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
blots -i '[1,2,3,4,5]' "output stats = {minimum: min(...inputs.value_1), maximum: max(...inputs.value_1)}" | jq

# Save output to file
blots "output data = range(1, 11) via (x => x^2)" -o squares.json
# Or:
blots "output data = range(1, 11) via (x => x^2)" > squares.json

# Chain Blots programs
blots "output nums = range(1, 6)" | blots "output squares = inputs.nums via x => x^2"
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
- `random(seed)` - returns a pseudo-random number in the range [0, 1) based on the given seed. The same seed always produces the same result, making it deterministic and reproducible. Use different seeds to generate different random numbers (e.g., `random(42)` always returns the same value, while `[1, 2, 3, 4, 5] via random` generates five different random numbers)

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
- `any(list)` - returns true if *any* element in the list is `true`
- `all(list)` - returns true if *all* elements in the list are `true`

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
- `includes(string, substring)` - checks if string contains substring
- `format(string, ...values)` - formats a string with placeholder values (e.g. `format("answer: {}", 42))

#### Type Functions
- `typeof(value)` - returns the type of a value ("number", "string", "boolean", "null", "list", "record", "built-in function", or "function")
- `arity(fn)` - returns the minimum number of parameters a function expects
- `to_string(value)` - converts a value to its string representation
- `to_number(value)` - converts a string or boolean to a number. If value is a string, parses it as a floating-point number. If value is a boolean, returns 1 for `true` and 0 for `false`.
- `to_bool(number)` - converts a number to a boolean. If the number is 0, then returns `false`. Otherwise, returns `true`.

#### Record Functions
- `keys(record)` - returns a list of all keys in a record
- `values(record)` - returns a list of all values in a record
- `entries(record)` - returns a list of [key, value] pairs from a record

#### Unit Conversion
- `convert(value, from_unit, to_unit)` - converts a numeric value from one unit to another

The `convert` function supports 200+ units across 19 categories:
- **Angle**: degrees, radians, gradians, revolutions, arc minutes, arc seconds
- **Area**: square meters/kilometers/miles/feet/etc., acres, hectares
- **Concentration of Mass**: grams per liter, milligrams per deciliter
- **Duration**: seconds, minutes, hours, days, weeks, milliseconds, etc.
- **Electric Charge**: coulombs, ampere hours (with metric prefixes)
- **Electric Current**: amperes (with metric prefixes)
- **Electric Potential Difference**: volts (with metric prefixes)
- **Electric Resistance**: ohms (with metric prefixes)
- **Energy**: joules, calories, kilocalories, kilowatt hours
- **Frequency**: hertz (with metric prefixes)
- **Fuel Efficiency**: liters per 100km, miles per gallon
- **Information Storage**: bits, bytes, kilobytes, kibibytes, megabytes, mebibytes, etc.
- **Length**: meters, kilometers, miles, feet, inches, nautical miles, light years, etc.
- **Mass**: kilograms, grams, pounds, ounces, tons, stones, etc.
- **Power**: watts (with metric prefixes), horsepower
- **Pressure**: pascals, bars, atmospheres, psi, mmHg, inHg
- **Speed**: meters per second, kilometers per hour, miles per hour, knots
- **Temperature**: Celsius, Fahrenheit, Kelvin
- **Volume**: liters, gallons, cubic meters, cups, pints, quarts, etc.

Units can be specified by full name or abbreviation and are case-insensitive. Metric units support both American ("meter") and British ("metre") spellings.

Examples:
```blots
convert(100, "celsius", "fahrenheit")  // 212
convert(5, "km", "miles")              // 3.1068559611866697
convert(1, "kg", "lbs")                // 2.2046226218487757
convert(1024, "bytes", "kibibytes")    // 1
convert(180, "degrees", "radians")     // 3.141592653589793 (π)
convert(1, "kilowatt", "watts")        // 1000
```

### Constants

Access mathematical constants via `constants.*`:
- `constants.pi`: The mathematical constant _π_.
- `constants.e`: The mathematical constant _e_.
- `constants.max_value`: The maximum value that can be represented as a 64-bit floating point number.
- `constants.min_value`: The minimum non-zero value that can be represented as a 64-bit floating point number.

## Tools

There's a language support [extension](https://github.com/paul-russo/blots-syntax) for Blots, available on both the [VSCode Marketplace](https://marketplace.visualstudio.com/items?itemName=Blots.blots-syntax) and the [Open VSX Registry](https://open-vsx.org/extension/blots/blots-syntax). You should be able to install it from within your editor like other extensions, but you can also download the VSIX file directly from either directory.

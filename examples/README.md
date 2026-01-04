# Dapper Examples

This directory contains example programs demonstrating the features of the Dapper programming language.

## Supported Types

- **int**: Integer numbers (e.g., `42`, `123`)
- **float**: Floating-point numbers (e.g., `3.14`, `2.0`)
- **bool**: Boolean values (`true`, `false`)
- **string**: String literals (e.g., `"Hello, World!"`)
- **unit**: Unit type, represented as `()`

## Supported Features

### Conditionals
- **if/then/else**: Conditional expressions that evaluate to a value
- Both branches must have the same type
- Condition must be a boolean expression

### Comparison Operators
- **==**: Equality
- **!=**: Inequality
- **<=**: Less than or equal
- **>=**: Greater than or equal

**Note**: `<` and `>` are currently reserved for generic type parameters (e.g., `foo<int>`). Use `<=` or `>=` for comparisons.

### Arithmetic Operators
- **+**: Addition (int and float)
- **-**: Subtraction (int and float)
- **\***: Multiplication (int and float)
- **/**: Division (int and float)

## Example Files

### test_float.dpr
Demonstrates float arithmetic operations:
```dapper
fun main() {
    let x = 3.14 in
    let y = 2.0 in
    x * y
}
```
Output: `Result = 6.28`

### test_bool.dpr
Demonstrates boolean literals:
```dapper
fun main() {
    true
}
```
Output: `Result = true`

### test_unit.dpr
Demonstrates unit type:
```dapper
fun main() {
    ()
}
```
Output: `Result = ()`

### test_mixed_types.dpr
Demonstrates functions with different parameter types:
```dapper
fun add_int(a, b) { a + b }
fun add_float(a, b) { a + b }

fun main() {
    let x = add_int(5, 3) in
    let y = add_float(2.5, 1.5) in
    x + 10
}
```
Output: `Result = 18`

### test_if_else.dpr
Demonstrates if/else conditional with a max function:
```dapper
fun max(a, b) {
    if a >= b then a else b
}

fun main() {
    max(10, 20)
}
```
Output: `Result = 20`

### test_comparison.dpr
Demonstrates equality comparison:
```dapper
fun main() {
    let x = 5 in
    let y = 10 in
    if x == y then 1 else 0
}
```
Output: `Result = 0`

### test_float_comparison.dpr
Demonstrates float comparison:
```dapper
fun main() {
    let x = 3.14 in
    let y = 2.71 in
    if x >= y then x else y
}
```
Output: `Result = 3.14`

### test_nested_if.dpr
Demonstrates nested if expressions:
```dapper
fun classify(x) {
    if x <= 0 then
        0
    else
        if x <= 10 then 1 else 2
}

fun main() {
    classify(5)
}
```
Output: `Result = 1`

## Running Examples

To run any example:
```bash
cargo run -- run examples/<filename>.dpr
```

For example:
```bash
cargo run -- run examples/test_float.dpr
```

## Type Inference

All examples use Hindley-Milner type inference. The compiler automatically infers:
- Parameter types
- Return types
- Variable types
- Proper LLVM types for code generation

Type information is displayed during compilation, showing the inferred type signature for each function.

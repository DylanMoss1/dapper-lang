# Dapper Programming Language

A statically-typed functional programming language with Hindley-Milner type inference and LLVM backend.

## Features

- **Automatic Type Inference**: Write code without type annotations, the compiler figures it out
- **Polymorphic Functions**: Generic functions with OCaml-style type parameters (`'a`, `'b`)
- **Type Safety**: Compile-time type checking catches errors early
- **LLVM Backend**: Efficient code generation via LLVM
- **JIT Execution**: Run programs instantly without explicit compilation

## Installation

```bash
cargo build --release
```

The compiler binary will be available at `target/release/dapper`.

## Usage

### Run a Dapper program

Compile and execute a Dapper source file:

```bash
dapper run <file>
```

Example:
```bash
dapper run examples/hello.dpr
```

Output:
```
Running: examples/hello.dpr
Type checking passed ✓
Inferred types:
  add: int -> int -> int
  main: int

Executing...
Result = 42
```

### Build a Dapper program

Compile a Dapper source file to LLVM IR:

```bash
dapper build <file> [-o <output>]
```

Examples:
```bash
# Output to default file (input.ll)
dapper build examples/hello.dpr

# Specify custom output file
dapper build examples/hello.dpr -o /tmp/hello.ll
```

## Language Syntax

### Basic Function

```dapper
fun add(x, y) {
    x + y
}

fun main() {
    add(10, 32)
}
```

The compiler automatically infers:
- `add: int -> int -> int`
- `main: int`

### Polymorphic Functions

Functions can be polymorphic (generic):

```dapper
fun id<'a>(x) {
    x
}

fun main() {
    id<int>(42)
}
```

The compiler infers:
- `id: forall 't0. 't0 -> 't0`
- `main: int`

### Let Bindings

```dapper
fun main() {
    let x = 5 in
    let y = 10 in
    x + y
}
```

### Type Inference

The type system automatically infers types based on usage:

```dapper
fun first(x, y) { x }
```

Inferred type: `forall 'a 'b. 'a -> 'b -> 'a`

## Example Programs

See the `examples/` directory for sample programs:

- `examples/hello.dpr` - Simple addition function
- `examples/polymorphic.dpr` - Generic identity function
- `examples/error.dpr` - Example showing type error

## Language Reference

### Supported Types

- `int` - 32-bit integers
- `bool` - Boolean values (true/false)
- `float` - 64-bit floating point
- `string` - String literals
- `unit` - Unit type (empty value)
- Type variables: `'a`, `'b`, etc.
- Function types: `T1 -> T2`

### Operators

- `+` - Addition
- `-` - Subtraction
- `*` - Multiplication
- `/` - Division

### Syntax

```
<module>     ::= <function>*
<function>   ::= "fun" <name> <type-params>? "(" <params> ")" "{" <expr> "}"
<type-params>::= "<" <type-var> ("," <type-var>)* ">"
<params>     ::= <name> ("," <name>)*
<expr>       ::= <number>
               | <identifier>
               | <expr> <op> <expr>
               | "let" <name> "=" <expr> "in" <expr>
               | <name> "(" <args> ")"
```

## Development

### Running Tests

```bash
cargo test
```

### Building from Source

```bash
cargo build
```

### Type Checker

The compiler uses a Hindley-Milner type inference system with:
- Unification algorithm for type solving
- Let-polymorphism for generic bindings
- Automatic generalization and instantiation

## Error Handling

The compiler provides clear error messages for type errors:

```bash
$ dapper run examples/error.dpr
Error: Type error: TooFewArgs
```

## License

MIT

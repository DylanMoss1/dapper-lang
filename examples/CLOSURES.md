# Closure Implementation

Closures have been implemented in the Dapper language with the syntax `|param1, param2, ...| { body }`.

## Syntax

```dapper
// Single-parameter closure
let identity = |x| { x } in ...

// Multi-parameter closure (desugars to nested closures)
let add = |x, y| { x + y } in ...

// Closure with type annotations
let typed = |x: int| { x + 1 } in ...
```

## Implementation Details

### Lexer (src/lexer.rs)
- Added `Pipe` token for the `|` character

### Parser (src/parser.lalrpop)
- Added closure syntax: `|params| { body }`
- Multi-parameter closures are automatically desugared into nested single-parameter lambdas
- Example: `|x, y| { x + y }` becomes `|x| |y| { x + y }`
- Braces are required around the body to avoid parser ambiguity

### Type Checker (src/typechecker.rs)
- Already supported `Expr::Lambda` for single-parameter lambdas
- Multi-parameter closures work automatically due to desugaring
- Type inference works correctly for closure types

### Code Generation (src/codegen.rs)
- Lambdas are compiled to LLVM functions with unique names (`lambda_0`, `lambda_1`, etc.)
- Each lambda becomes a separate LLVM function with proper parameter handling

## Current Limitations

1. **No Variable Capture**: Closures cannot capture variables from outer scopes. Only the lambda parameters are accessible in the body.

2. **No First-Class Functions**: Lambdas are created but cannot be called directly. Full first-class function support (function pointers, higher-order functions) requires additional infrastructure.

3. **Multi-Parameter Limitations**: While multi-parameter closures parse and type-check correctly, using them requires first-class function support due to the nested lambda desugaring.

## Working Examples

### Single-Parameter Closures

```dapper
fun main() {
    let identity = |x| { x } in
    let double = |x| { x + x } in
    let triple = |x| { x + x + x } in
    42
}
```

This example successfully compiles and generates three lambda functions in LLVM IR.

## Generated LLVM IR

For the example above, the following LLVM functions are generated:

```llvm
define i32 @lambda_0(i32 %0) {  ; identity
entry:
  %x = alloca i32, align 4
  store i32 %0, ptr %x, align 4
  %x1 = load i32, ptr %x, align 4
  ret i32 %x1
}

define i32 @lambda_1(i32 %0) {  ; double
entry:
  %x = alloca i32, align 4
  store i32 %0, ptr %x, align 4
  %x1 = load i32, ptr %x, align 4
  %x2 = load i32, ptr %x, align 4
  %tmpadd = add i32 %x1, %x2
  ret i32 %tmpadd
}

define i32 @lambda_2(i32 %0) {  ; triple
entry:
  %x = alloca i32, align 4
  store i32 %0, ptr %x, align 4
  %x1 = load i32, ptr %x, align 4
  %x2 = load i32, ptr %x, align 4
  %tmpadd = add i32 %x1, %x2
  %x3 = load i32, ptr %x, align 4
  %tmpadd4 = add i32 %tmpadd, %x3
  ret i32 %tmpadd4
}
```

## Future Enhancements

To make closures fully functional:

1. **Closure Capture**: Implement proper variable capture by creating closure structs that contain captured variables
2. **Function Pointers**: Add support for function pointer types in the type system
3. **Higher-Order Functions**: Allow passing closures as arguments and returning them from functions
4. **Call Expressions**: Add syntax for calling function values (not just named functions)

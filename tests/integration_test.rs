use dapper::{Expr, LexerBridge, Module, ModuleParser, Opcode, Token, TypeChecker, Var, TypeDef, EnumVariant};
use logos::Logos;

fn parse_and_check(input: &str) -> Module {
    let lexer = Token::lexer(input);
    let lexer_bridge = LexerBridge { lexer };
    let parser = ModuleParser::new();
    parser.parse(lexer_bridge).expect("Failed to parse")
}

#[test]
fn test_simple_function_no_params() {
    let input = "fun answer() { 42 }";
    let module = parse_and_check(input);

    assert_eq!(module.functions.len(), 1);
    assert_eq!(module.functions[0].name, Var("answer".to_string()));
    assert_eq!(module.functions[0].params.len(), 0);

    match &module.functions[0].body {
        Expr::Number(n) => assert_eq!(*n, 42),
        _ => panic!("Expected number expression"),
    }
}

#[test]
fn test_function_with_one_param() {
    let input = "fun double(x) { x + x }";
    let module = parse_and_check(input);

    assert_eq!(module.functions.len(), 1);
    assert_eq!(module.functions[0].params.len(), 1);
    assert_eq!(module.functions[0].params[0].0, Var("x".to_string()));
}

#[test]
fn test_function_with_multiple_params() {
    let input = "fun add(a, b) { a + b }";
    let module = parse_and_check(input);

    assert_eq!(module.functions.len(), 1);
    assert_eq!(module.functions[0].params.len(), 2);
    assert_eq!(module.functions[0].params[0].0, Var("a".to_string()));
    assert_eq!(module.functions[0].params[1].0, Var("b".to_string()));
}

#[test]
fn test_function_call_with_args() {
    let input = "fun add(a, b) { a + b } fun main() { add(1, 2) }";
    let module = parse_and_check(input);

    assert_eq!(module.functions.len(), 2);

    // Check main function body contains a function call
    match &module.functions[1].body {
        Expr::Apply { fun_name, args, .. } => {
            assert_eq!(*fun_name, Var("add".to_string()));
            assert_eq!(args.len(), 2);
        }
        _ => panic!("Expected function call"),
    }
}

#[test]
fn test_nested_function_calls() {
    let input = "fun add(a, b) { a + b } fun main() { add(add(1, 2), 3) }";
    let module = parse_and_check(input);

    assert_eq!(module.functions.len(), 2);
}

#[test]
fn test_arithmetic_operators() {
    let test_cases = vec![
        ("fun main() { 1 + 2 }", Opcode::Add),
        ("fun main() { 1 - 2 }", Opcode::Sub),
        ("fun main() { 1 * 2 }", Opcode::Mul),
        ("fun main() { 1 / 2 }", Opcode::Div),
    ];

    for (input, _expected_op) in test_cases {
        let module = parse_and_check(input);
        match &module.functions[0].body {
            Expr::Op { .. } => {
                // Successfully parsed as an operation
            }
            _ => panic!("Expected Op expression"),
        }
    }
}

#[test]
fn test_let_binding() {
    let input = "fun main() { let x = 5 in x + 1 }";
    let module = parse_and_check(input);

    match &module.functions[0].body {
        Expr::LetBinding { bound_var, bound_expr, body } => {
            assert_eq!(*bound_var, Var("x".to_string()));
            assert!(matches!(**bound_expr, Expr::Number(5)));
            assert!(matches!(**body, Expr::Op { .. }));
        }
        _ => panic!("Expected LetBinding"),
    }
}

#[test]
fn test_operator_precedence_multiplication_before_addition() {
    let input = "fun main() { 1 + 2 * 3 }";
    let module = parse_and_check(input);

    // Should parse as 1 + (2 * 3)
    match &module.functions[0].body {
        Expr::Op { op, left, right } => {
            assert!(matches!(op, Opcode::Add));
            assert!(matches!(**left, Expr::Number(1)));
            // Right side should be multiplication
            match &**right {
                Expr::Op { op, .. } => assert!(matches!(op, Opcode::Mul)),
                _ => panic!("Expected multiplication on right side"),
            }
        }
        _ => panic!("Expected Op expression"),
    }
}

#[test]
fn test_multiple_functions_in_module() {
    let input = "fun add(a, b) { a + b } fun sub(x, y) { x - y } fun main() { add(1, 2) }";
    let module = parse_and_check(input);

    assert_eq!(module.functions.len(), 3);
    assert_eq!(module.functions[0].name, Var("add".to_string()));
    assert_eq!(module.functions[1].name, Var("sub".to_string()));
    assert_eq!(module.functions[2].name, Var("main".to_string()));
}

#[test]
fn test_function_with_three_params() {
    let input = "fun add3(a, b, c) { a + b + c }";
    let module = parse_and_check(input);

    assert_eq!(module.functions[0].params.len(), 3);
    assert_eq!(module.functions[0].params[0].0, Var("a".to_string()));
    assert_eq!(module.functions[0].params[1].0, Var("b".to_string()));
    assert_eq!(module.functions[0].params[2].0, Var("c".to_string()));
}

#[test]
fn test_complex_expression_with_multiple_operations() {
    let input = "fun main() { 10 + 20 * 30 - 40 / 5 }";
    let module = parse_and_check(input);

    // Just verify it parses successfully
    assert_eq!(module.functions.len(), 1);
}

#[test]
fn test_function_call_with_no_args() {
    let input = "fun get_value() { 42 } fun main() { get_value() }";
    let module = parse_and_check(input);

    assert_eq!(module.functions.len(), 2);
    match &module.functions[1].body {
        Expr::Apply { fun_name, args, .. } => {
            assert_eq!(*fun_name, Var("get_value".to_string()));
            assert_eq!(args.len(), 0);
        }
        _ => panic!("Expected function call"),
    }
}

#[test]
fn test_nested_let_bindings() {
    let input = "fun main() { let x = 1 in let y = 2 in x + y }";
    let module = parse_and_check(input);

    match &module.functions[0].body {
        Expr::LetBinding { bound_var, body, .. } => {
            assert_eq!(*bound_var, Var("x".to_string()));
            // Inner body should also be a let binding
            assert!(matches!(**body, Expr::LetBinding { .. }));
        }
        _ => panic!("Expected LetBinding"),
    }
}

#[test]
fn test_function_call_as_argument() {
    let input = "fun id(x) { x } fun main() { id(id(5)) }";
    let module = parse_and_check(input);

    assert_eq!(module.functions.len(), 2);
    match &module.functions[1].body {
        Expr::Apply { args, .. } => {
            assert_eq!(args.len(), 1);
            // Argument should be another function call
            assert!(matches!(args[0], Expr::Apply { .. }));
        }
        _ => panic!("Expected function call"),
    }
}

#[test]
fn test_empty_module() {
    let input = "";
    let module = parse_and_check(input);
    assert_eq!(module.functions.len(), 0);
}

#[test]
fn test_generic_function_definition() {
    let input = "fun id<'a>(x) { x } fun main() { id(5) }";
    let module = parse_and_check(input);

    assert_eq!(module.functions[0].type_params, vec!["a".to_string()]);
    assert_eq!(module.functions[0].params.len(), 1);
}

#[test]
fn test_multiple_type_parameters() {
    let input = "fun choose<'a, 'b>(x, y) { x } fun main() { choose(1, 2) }";
    let module = parse_and_check(input);

    assert_eq!(module.functions[0].type_params.len(), 2);
    assert_eq!(module.functions[0].type_params[0], "a");
    assert_eq!(module.functions[0].type_params[1], "b");
}

#[test]
fn test_generic_and_non_generic_functions() {
    let input = "fun id<'a>(x) { x } fun add(a, b) { a + b } fun main() { id(5) + add(1, 2) }";
    let module = parse_and_check(input);

    assert_eq!(module.functions[0].type_params.len(), 1);
    assert_eq!(module.functions[1].type_params.len(), 0);
    assert_eq!(module.functions[2].type_params.len(), 0);
}

#[test]
fn test_generic_function_call_with_type_args() {
    let input = "fun id<'a>(x) { x } fun main() { id<int>(5) }";
    let module = parse_and_check(input);

    match &module.functions[1].body {
        Expr::Apply { type_args, .. } => {
            assert_eq!(type_args.len(), 1);
            assert_eq!(type_args[0], "int");
        }
        _ => panic!("Expected Apply expression"),
    }
}

#[test]
fn test_generic_function_call_without_type_args() {
    let input = "fun id<'a>(x) { x } fun main() { id(5) }";
    let module = parse_and_check(input);

    match &module.functions[1].body {
        Expr::Apply { type_args, .. } => {
            assert!(type_args.is_empty());
        }
        _ => panic!("Expected Apply expression"),
    }
}

#[test]
fn test_nested_generic_calls() {
    let input = "fun id<'a>(x) { x } fun main() { id<int>(id<int>(5)) }";
    let module = parse_and_check(input);

    match &module.functions[1].body {
        Expr::Apply { type_args, args, .. } => {
            assert_eq!(type_args.len(), 1);
            assert_eq!(args.len(), 1);
            // Nested call should also have type args
            match &args[0] {
                Expr::Apply { type_args, .. } => {
                    assert_eq!(type_args.len(), 1);
                }
                _ => panic!("Expected nested Apply"),
            }
        }
        _ => panic!("Expected Apply expression"),
    }
}

// Type Inference Tests

#[test]
fn test_type_inference_simple_identity() {
    // fun id(x) { x } should infer as: forall 'a. 'a -> 'a
    let input = "fun id(x) { x }";
    let module = parse_and_check(input);

    let mut checker = TypeChecker::new();
    let result = checker.check_module(&module);

    assert!(result.is_ok());
    let env = result.unwrap();

    let id_scheme = env.lookup(&Var("id".to_string())).unwrap();
    // Should have at least one quantified variable (polymorphic)
    assert!(!id_scheme.quantifiers.is_empty());
}

#[test]
fn test_type_inference_add_function() {
    // fun add(x, y) { x + y } should infer as: int -> int -> int
    let input = "fun add(x, y) { x + y }";
    let module = parse_and_check(input);

    let mut checker = TypeChecker::new();
    let result = checker.check_module(&module);

    assert!(result.is_ok());
}

#[test]
fn test_type_inference_first_function() {
    // fun first(x, y) { x } should infer as: forall 'a 'b. 'a -> 'b -> 'a
    let input = "fun first(x, y) { x }";
    let module = parse_and_check(input);

    let mut checker = TypeChecker::new();
    let result = checker.check_module(&module);

    assert!(result.is_ok());
    let env = result.unwrap();

    let first_scheme = env.lookup(&Var("first".to_string())).unwrap();
    // Should be polymorphic
    assert!(!first_scheme.quantifiers.is_empty());
}

#[test]
fn test_type_inference_with_annotations() {
    // fun add(x: int, y: int) -> int { x + y }
    let input = "fun add(x: int, y: int) -> int { x + y }";
    let module = parse_and_check(input);

    let mut checker = TypeChecker::new();
    let result = checker.check_module(&module);

    assert!(result.is_ok());
}

#[test]
fn test_type_inference_let_polymorphism() {
    // fun main() { let x = 5 in x + x }
    let input = "fun main() { let x = 5 in x + x }";
    let module = parse_and_check(input);

    let mut checker = TypeChecker::new();
    let result = checker.check_module(&module);

    assert!(result.is_ok());
}

#[test]
fn test_type_inference_function_call() {
    let input = "fun id(x) { x } fun main() { id(42) }";
    let module = parse_and_check(input);

    let mut checker = TypeChecker::new();
    let result = checker.check_module(&module);

    assert!(result.is_ok());
}

#[test]
fn test_type_inference_nested_calls() {
    let input = "fun id(x) { x } fun main() { id(id(5)) }";
    let module = parse_and_check(input);

    let mut checker = TypeChecker::new();
    let result = checker.check_module(&module);

    assert!(result.is_ok());
}

// Enum Tests

#[test]
fn test_simple_enum_definition() {
    let input = "type Tree<'a> = Leaf | Branch('a, Tree<'a>, Tree<'a>)";
    let module = parse_and_check(input);

    assert_eq!(module.type_defs.len(), 1);
    assert_eq!(module.type_defs[0].name, "Tree");
    assert_eq!(module.type_defs[0].type_params.len(), 1);
    assert_eq!(module.type_defs[0].type_params[0], "a");
    assert_eq!(module.type_defs[0].variants.len(), 2);

    // Check first variant (Leaf)
    assert_eq!(module.type_defs[0].variants[0].name, "Leaf");
    assert_eq!(module.type_defs[0].variants[0].fields.len(), 0);

    // Check second variant (Branch)
    assert_eq!(module.type_defs[0].variants[1].name, "Branch");
    assert_eq!(module.type_defs[0].variants[1].fields.len(), 3);
}

#[test]
fn test_enum_without_type_params() {
    let input = "type Bool = True | False";
    let module = parse_and_check(input);

    assert_eq!(module.type_defs.len(), 1);
    assert_eq!(module.type_defs[0].name, "Bool");
    assert_eq!(module.type_defs[0].type_params.len(), 0);
    assert_eq!(module.type_defs[0].variants.len(), 2);
}

#[test]
fn test_enum_with_functions() {
    let input = "type Option<'a> = None | Some('a) fun makeNone() { None() } fun makeSome(x: int) { Some(x) }";
    let module = parse_and_check(input);

    assert_eq!(module.type_defs.len(), 1);
    assert_eq!(module.functions.len(), 2);
    assert_eq!(module.type_defs[0].name, "Option");
    assert_eq!(module.type_defs[0].variants.len(), 2);
}

#[test]
fn test_multiple_enum_definitions() {
    let input = "type Option<'a> = None | Some('a) type List<'a> = Nil | Cons('a, List<'a>)";
    let module = parse_and_check(input);

    assert_eq!(module.type_defs.len(), 2);
    assert_eq!(module.type_defs[0].name, "Option");
    assert_eq!(module.type_defs[1].name, "List");
}

#[test]
fn test_enum_type_checking() {
    let input = "type Tree<'a> = Leaf | Branch('a, Tree<'a>, Tree<'a>) fun makeLeaf() { Leaf() }";
    let module = parse_and_check(input);

    let mut checker = TypeChecker::new();
    let result = checker.check_module(&module);

    assert!(result.is_ok());
}

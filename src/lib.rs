use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub parser);

pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod types;
pub mod typechecker;

pub use crate::ast::{Expr, Function, Module, Opcode, TypeAnnotation, Var, TypeDef, EnumVariant};
pub use crate::lexer::{LexerBridge, Token};
pub use crate::parser::ModuleParser;
pub use crate::types::{Type, TypeEnv, TypeScheme, TypeVar};
pub use crate::typechecker::{FunctionUsage, TypeChecker, TypeError, UsageAnalyzer};

#[cfg(test)]
mod tests {
    use super::*;
    use logos::Logos;

    fn parse(input: &str) -> Result<Module, String> {
        let lexer = Token::lexer(input);
        let lexer_bridge = LexerBridge { lexer };
        let parser = ModuleParser::new();
        parser.parse(lexer_bridge).map_err(|e| format!("{:?}", e))
    }

    #[test]
    fn test_parse_simple_number() {
        let result = parse("fun main() { 42 }");
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].name, Var("main".to_string()));
    }

    #[test]
    fn test_parse_arithmetic() {
        let result = parse("fun main() { 1 + 2 * 3 }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_function_no_params() {
        let result = parse("fun foo() { 10 }");
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].params.len(), 0);
    }

    #[test]
    fn test_parse_function_one_param() {
        let result = parse("fun double(x) { x + x }");
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].params.len(), 1);
        assert_eq!(module.functions[0].params[0].0, Var("x".to_string()));
    }

    #[test]
    fn test_parse_function_multiple_params() {
        let result = parse("fun add(a, b, c) { a + b + c }");
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[0].params.len(), 3);
        assert_eq!(module.functions[0].params[0].0, Var("a".to_string()));
        assert_eq!(module.functions[0].params[1].0, Var("b".to_string()));
        assert_eq!(module.functions[0].params[2].0, Var("c".to_string()));
    }

    #[test]
    fn test_parse_function_call_no_args() {
        let result = parse("fun main() { foo() }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_function_call_one_arg() {
        let result = parse("fun main() { foo(1) }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_function_call_multiple_args() {
        let result = parse("fun main() { add(1, 2, 3) }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_let_binding() {
        let result = parse("fun main() { let x = 5 in x + 1 }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_nested_let_binding() {
        let result = parse("fun main() { let x = 1 in let y = 2 in x + y }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_multiple_functions() {
        let result = parse("fun add(a, b) { a + b } fun main() { add(1, 2) }");
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.functions.len(), 2);
        assert_eq!(module.functions[0].name, Var("add".to_string()));
        assert_eq!(module.functions[1].name, Var("main".to_string()));
    }

    #[test]
    fn test_parse_complex_expression() {
        // Note: parentheses for grouping are not yet supported
        let result = parse("fun main() { 1 + 2 * 3 - 4 / 5 }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_operator_precedence() {
        let result = parse("fun main() { 1 + 2 * 3 }");
        assert!(result.is_ok());
        let module = result.unwrap();
        // Verify it parses as 1 + (2 * 3), not (1 + 2) * 3
        match &module.functions[0].body {
            Expr::Op { op, left, right } => {
                assert!(matches!(op, Opcode::Add));
                assert!(matches!(**left, Expr::Number(1)));
                assert!(matches!(**right, Expr::Op { .. }));
            }
            _ => panic!("Expected Op expression"),
        }
    }

    #[test]
    fn test_parse_all_operators() {
        assert!(parse("fun main() { 1 + 2 }").is_ok());
        assert!(parse("fun main() { 1 - 2 }").is_ok());
        assert!(parse("fun main() { 1 * 2 }").is_ok());
        assert!(parse("fun main() { 1 / 2 }").is_ok());
    }

    #[test]
    fn test_parse_trailing_comma_allowed() {
        // Trailing commas are allowed (similar to Rust)
        let result = parse("fun add(a, b,) { a + b }");
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.functions[0].params.len(), 2);
    }

    #[test]
    fn test_parse_empty_module() {
        let result = parse("");
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.functions.len(), 0);
    }

    #[test]
    fn test_parse_generic_function_one_param() {
        let result = parse("fun id<'a>(x) { x }");
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.functions[0].type_params, vec!["a".to_string()]);
        assert_eq!(module.functions[0].params.len(), 1);
    }

    #[test]
    fn test_parse_generic_function_multiple_params() {
        let result = parse("fun pair<'a, 'b>(a, b) { a + b }");
        assert!(result.is_ok());
        let module = result.unwrap();
        assert_eq!(module.functions[0].type_params.len(), 2);
        assert_eq!(module.functions[0].type_params[0], "a");
        assert_eq!(module.functions[0].type_params[1], "b");
    }

    #[test]
    fn test_parse_non_generic_function() {
        let result = parse("fun add(a, b) { a + b }");
        assert!(result.is_ok());
        let module = result.unwrap();
        assert!(module.functions[0].type_params.is_empty());
    }

    #[test]
    fn test_parse_generic_function_call_with_type_args() {
        let result = parse("fun main() { id<int>(5) }");
        assert!(result.is_ok());
        let module = result.unwrap();
        match &module.functions[0].body {
            Expr::Apply { type_args, .. } => {
                assert_eq!(type_args.len(), 1);
                assert_eq!(type_args[0], "int");
            }
            _ => panic!("Expected Apply expression"),
        }
    }

    #[test]
    fn test_parse_generic_function_call_without_type_args() {
        let result = parse("fun main() { id(5) }");
        assert!(result.is_ok());
        let module = result.unwrap();
        match &module.functions[0].body {
            Expr::Apply { type_args, .. } => {
                assert!(type_args.is_empty());
            }
            _ => panic!("Expected Apply expression"),
        }
    }

    #[test]
    fn test_parse_generic_function_multiple_type_args() {
        let result = parse("fun main() { pair<int, bool>(1, 2) }");
        assert!(result.is_ok());
        let module = result.unwrap();
        match &module.functions[0].body {
            Expr::Apply { type_args, .. } => {
                assert_eq!(type_args.len(), 2);
                assert_eq!(type_args[0], "int");
                assert_eq!(type_args[1], "bool");
            }
            _ => panic!("Expected Apply expression"),
        }
    }
}

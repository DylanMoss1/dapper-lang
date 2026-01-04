#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Var(pub String);

/// Type annotation syntax for function signatures
#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotation {
    TName(String),                                    // int, bool, string, unit, float
    TArrow(Box<TypeAnnotation>, Box<TypeAnnotation>), // T1 -> T2
    TVar(String),                                     // Generic type variable 'a, 'b, etc.
}

#[derive(Debug)]
pub enum Expr {
    // Let bindings
    LetBinding {
        bound_var: Var,
        bound_expr: Box<Expr>,
        body: Box<Expr>,
    },

    // Literals
    Number(i32),
    Float(f64),
    Bool(bool),
    String(String),
    Unit,

    // Partial application placeholder
    PartialPlaceholder,

    // Variables
    Var(Var),

    // Binary operations
    Op {
        op: Opcode,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    // Conditional expression
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },

    // Function application
    Apply {
        fun_name: Var,
        type_args: Vec<String>,
        args: Vec<Expr>,
        partial: bool, // true if partial application (ends with *)
    },

    // Lambda expressions (for partial application)
    Lambda {
        param: Var,
        param_type: Option<TypeAnnotation>,
        body: Box<Expr>,
    },
}

#[derive(Debug)]
pub enum Opcode {
    // Arithmetic
    Mul,
    Div,
    Add,
    Sub,

    // Comparison
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
}

#[derive(Debug)]
pub struct Function {
    pub name: Var,
    pub type_params: Vec<String>,
    pub params: Vec<(Var, Option<TypeAnnotation>)>, // Parameters with optional type annotations
    pub return_type: Option<TypeAnnotation>,        // Optional return type annotation
    pub body: Expr,
}

#[derive(Debug)]
pub struct Module(pub Vec<Function>);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_var_equality() {
        let var1 = Var("x".to_string());
        let var2 = Var("x".to_string());
        let var3 = Var("y".to_string());

        assert_eq!(var1, var2);
        assert_ne!(var1, var3);
    }

    #[test]
    fn test_var_clone() {
        let var1 = Var("test".to_string());
        let var2 = var1.clone();

        assert_eq!(var1, var2);
    }

    #[test]
    fn test_function_structure() {
        let func = Function {
            name: Var("add".to_string()),
            type_params: vec![],
            params: vec![(Var("a".to_string()), None), (Var("b".to_string()), None)],
            return_type: None,
            body: Expr::Op {
                op: Opcode::Add,
                left: Box::new(Expr::Var(Var("a".to_string()))),
                right: Box::new(Expr::Var(Var("b".to_string()))),
            },
        };

        assert_eq!(func.name, Var("add".to_string()));
        assert_eq!(func.params.len(), 2);
        assert_eq!(func.params[0].0, Var("a".to_string()));
        assert_eq!(func.params[1].0, Var("b".to_string()));
    }

    #[test]
    fn test_module_with_multiple_functions() {
        let func1 = Function {
            name: Var("f1".to_string()),
            type_params: vec![],
            params: vec![],
            return_type: None,
            body: Expr::Number(1),
        };

        let func2 = Function {
            name: Var("f2".to_string()),
            type_params: vec![],
            params: vec![(Var("x".to_string()), None)],
            return_type: None,
            body: Expr::Var(Var("x".to_string())),
        };

        let module = Module(vec![func1, func2]);
        assert_eq!(module.0.len(), 2);
    }

    #[test]
    fn test_expr_number() {
        let expr = Expr::Number(42);
        match expr {
            Expr::Number(n) => assert_eq!(n, 42),
            _ => panic!("Expected Number variant"),
        }
    }

    #[test]
    fn test_expr_var() {
        let expr = Expr::Var(Var("x".to_string()));
        match expr {
            Expr::Var(v) => assert_eq!(v, Var("x".to_string())),
            _ => panic!("Expected Var variant"),
        }
    }

    #[test]
    fn test_expr_op() {
        let expr = Expr::Op {
            op: Opcode::Add,
            left: Box::new(Expr::Number(1)),
            right: Box::new(Expr::Number(2)),
        };

        match expr {
            Expr::Op { op, left, right } => {
                assert!(matches!(op, Opcode::Add));
                assert!(matches!(*left, Expr::Number(1)));
                assert!(matches!(*right, Expr::Number(2)));
            }
            _ => panic!("Expected Op variant"),
        }
    }

    #[test]
    fn test_expr_apply() {
        let expr = Expr::Apply {
            fun_name: Var("foo".to_string()),
            type_args: vec![],
            args: vec![Expr::Number(1), Expr::Number(2)],
            partial: false,
        };

        match expr {
            Expr::Apply {
                fun_name,
                type_args,
                args,
                partial,
            } => {
                assert_eq!(fun_name, Var("foo".to_string()));
                assert_eq!(type_args.len(), 0);
                assert_eq!(args.len(), 2);
                assert!(!partial);
            }
            _ => panic!("Expected Apply variant"),
        }
    }

    #[test]
    fn test_expr_let_binding() {
        let expr = Expr::LetBinding {
            bound_var: Var("x".to_string()),
            bound_expr: Box::new(Expr::Number(5)),
            body: Box::new(Expr::Var(Var("x".to_string()))),
        };

        match expr {
            Expr::LetBinding {
                bound_var,
                bound_expr,
                body,
            } => {
                assert_eq!(bound_var, Var("x".to_string()));
                assert!(matches!(*bound_expr, Expr::Number(5)));
                assert!(matches!(*body, Expr::Var(_)));
            }
            _ => panic!("Expected LetBinding variant"),
        }
    }

    #[test]
    fn test_generic_function_structure() {
        let func = Function {
            name: Var("id".to_string()),
            type_params: vec!["a".to_string()],
            params: vec![(Var("x".to_string()), None)],
            return_type: None,
            body: Expr::Var(Var("x".to_string())),
        };

        assert_eq!(func.type_params.len(), 1);
        assert_eq!(func.type_params[0], "a");
    }

    #[test]
    fn test_generic_function_multiple_type_params() {
        let func = Function {
            name: Var("pair".to_string()),
            type_params: vec!["a".to_string(), "b".to_string()],
            params: vec![(Var("a".to_string()), None), (Var("b".to_string()), None)],
            return_type: None,
            body: Expr::Var(Var("a".to_string())),
        };

        assert_eq!(func.type_params.len(), 2);
        assert_eq!(func.type_params[0], "a");
        assert_eq!(func.type_params[1], "b");
    }

    #[test]
    fn test_apply_with_type_args() {
        let expr = Expr::Apply {
            fun_name: Var("id".to_string()),
            type_args: vec!["int".to_string()],
            args: vec![Expr::Number(5)],
            partial: false,
        };

        match expr {
            Expr::Apply {
                fun_name,
                type_args,
                args,
                ..
            } => {
                assert_eq!(fun_name, Var("id".to_string()));
                assert_eq!(type_args.len(), 1);
                assert_eq!(type_args[0], "int");
                assert_eq!(args.len(), 1);
            }
            _ => panic!("Expected Apply variant"),
        }
    }
}

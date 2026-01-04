#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Var(pub String);

#[derive(Debug)]
pub enum Expr {
    LetBinding {
        bound_var: Var,
        bound_expr: Box<Expr>,
        body: Box<Expr>,
    },
    Number(i32),
    Var(Var),
    Op {
        op: Opcode,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Apply {
        fun_name: Var,
        args: Vec<Expr>,
    },
}

#[derive(Debug)]
pub enum Opcode {
    Mul,
    Div,
    Add,
    Sub,
}

#[derive(Debug)]
pub struct Function {
    pub name: Var,
    pub params: Vec<Var>,
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
            params: vec![Var("a".to_string()), Var("b".to_string())],
            body: Expr::Op {
                op: Opcode::Add,
                left: Box::new(Expr::Var(Var("a".to_string()))),
                right: Box::new(Expr::Var(Var("b".to_string()))),
            },
        };

        assert_eq!(func.name, Var("add".to_string()));
        assert_eq!(func.params.len(), 2);
        assert_eq!(func.params[0], Var("a".to_string()));
        assert_eq!(func.params[1], Var("b".to_string()));
    }

    #[test]
    fn test_module_with_multiple_functions() {
        let func1 = Function {
            name: Var("f1".to_string()),
            params: vec![],
            body: Expr::Number(1),
        };

        let func2 = Function {
            name: Var("f2".to_string()),
            params: vec![Var("x".to_string())],
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
            args: vec![Expr::Number(1), Expr::Number(2)],
        };

        match expr {
            Expr::Apply { fun_name, args } => {
                assert_eq!(fun_name, Var("foo".to_string()));
                assert_eq!(args.len(), 2);
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
}

use crate::ast::{Expr, Function, Module, TypeAnnotation, Var};
use crate::types::{Type, TypeEnv, TypeScheme, TypeVar};
use std::collections::HashMap;

#[derive(Debug)]
pub enum TypeError {
    Mismatch(Type, Type),
    UnboundVariable(Var),
    InfiniteType,
    TooManyArgs,
    TooFewArgs,
    NotAFunction(Type),
    UnknownType(String),
}

impl TypeError {
    pub fn report(&self) -> String {
        match self {
            TypeError::Mismatch(expected, got) => {
                format!(
                    "Type mismatch: expected {}, got {}",
                    expected.display(),
                    got.display()
                )
            }
            TypeError::UnboundVariable(v) => {
                format!("Unbound variable: {}", v.0)
            }
            TypeError::InfiniteType => "Infinite type detected".to_string(),
            TypeError::TooManyArgs => "Too many arguments".to_string(),
            TypeError::TooFewArgs => "Too few arguments".to_string(),
            TypeError::NotAFunction(t) => {
                format!("Not a function: {}", t.display())
            }
            TypeError::UnknownType(name) => {
                format!("Unknown type: {}", name)
            }
        }
    }
}

pub struct TypeChecker {
    // Fresh type variable generation
    next_var_id: usize,

    // Substitution from unification
    substitution: HashMap<TypeVar, Type>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            next_var_id: 0,
            substitution: HashMap::new(),
        }
    }

    /// Generate a fresh type variable
    fn fresh_var(&mut self) -> TypeVar {
        let id = self.next_var_id;
        self.next_var_id += 1;
        TypeVar::new(id)
    }

    /// Apply current substitution to a type
    pub fn apply_substitution(&self, ty: &Type) -> Type {
        match ty {
            Type::TVar(v) => {
                if let Some(t) = self.substitution.get(v) {
                    // Recursively apply substitution
                    self.apply_substitution(t)
                } else {
                    ty.clone()
                }
            }
            Type::TArrow(t1, t2) => Type::TArrow(
                Box::new(self.apply_substitution(t1)),
                Box::new(self.apply_substitution(t2)),
            ),
            Type::TForall(vars, t) => {
                Type::TForall(vars.clone(), Box::new(self.apply_substitution(t)))
            }
            _ => ty.clone(),
        }
    }

    /// Occurs check: does variable v occur in type t?
    fn occurs_check(&self, v: &TypeVar, t: &Type) -> bool {
        let t = self.apply_substitution(t);
        match t {
            Type::TVar(ref tv) => tv == v,
            Type::TArrow(t1, t2) => self.occurs_check(v, &t1) || self.occurs_check(v, &t2),
            Type::TForall(_, t) => self.occurs_check(v, &t),
            _ => false,
        }
    }

    /// Unification algorithm (Robinson's algorithm)
    pub fn unify(&mut self, t1: Type, t2: Type) -> Result<(), TypeError> {
        let t1 = self.apply_substitution(&t1);
        let t2 = self.apply_substitution(&t2);

        match (t1.clone(), t2.clone()) {
            // Same base types unify
            (Type::TInt, Type::TInt) => Ok(()),
            (Type::TBool, Type::TBool) => Ok(()),
            (Type::TString, Type::TString) => Ok(()),
            (Type::TUnit, Type::TUnit) => Ok(()),
            (Type::TFloat, Type::TFloat) => Ok(()),

            // Type variable unification
            (Type::TVar(v), t) | (t, Type::TVar(v)) => {
                if Type::TVar(v.clone()) == t {
                    Ok(())
                } else if self.occurs_check(&v, &t) {
                    Err(TypeError::InfiniteType)
                } else {
                    self.substitution.insert(v, t);
                    Ok(())
                }
            }

            // Function type unification
            (Type::TArrow(a1, r1), Type::TArrow(a2, r2)) => {
                self.unify(*a1, *a2)?;
                self.unify(*r1, *r2)
            }

            // Mismatch
            _ => Err(TypeError::Mismatch(t1, t2)),
        }
    }

    /// Convert TypeAnnotation to Type
    fn annotation_to_type(
        &mut self,
        ann: &TypeAnnotation,
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        match ann {
            TypeAnnotation::TName(name) => match name.as_str() {
                "int" => Ok(Type::TInt),
                "bool" => Ok(Type::TBool),
                "string" => Ok(Type::TString),
                "unit" => Ok(Type::TUnit),
                "float" => Ok(Type::TFloat),
                _ => Err(TypeError::UnknownType(name.clone())),
            },
            TypeAnnotation::TArrow(t1, t2) => {
                let ty1 = self.annotation_to_type(t1, env)?;
                let ty2 = self.annotation_to_type(t2, env)?;
                Ok(Type::TArrow(Box::new(ty1), Box::new(ty2)))
            }
            TypeAnnotation::TVar(name) => {
                // Type variable like 'a
                // Check if it's a declared type parameter
                if env.has_type_param(name) {
                    // For now, create a fresh type variable
                    // In a more complete implementation, we'd track the mapping
                    Ok(Type::TVar(self.fresh_var()))
                } else {
                    Err(TypeError::UnknownType(format!("'{}", name)))
                }
            }
        }
    }

    /// Generalize a type: quantify free type variables
    fn generalize(&self, ty: Type, env: &TypeEnv) -> TypeScheme {
        let ty = self.apply_substitution(&ty);
        let free_in_env = env.free_type_vars();
        let free_in_ty = ty.free_type_vars();
        let quantifiers: Vec<_> = free_in_ty.difference(&free_in_env).cloned().collect();

        TypeScheme { quantifiers, ty }
    }

    /// Instantiate a type scheme: replace quantified variables with fresh ones
    fn instantiate(&mut self, scheme: &TypeScheme) -> Type {
        let subst: HashMap<TypeVar, Type> = scheme
            .quantifiers
            .iter()
            .map(|v| (v.clone(), Type::TVar(self.fresh_var())))
            .collect();

        self.instantiate_with_subst(&scheme.ty, &subst)
    }

    /// Helper to instantiate with a given substitution
    fn instantiate_with_subst(&self, ty: &Type, subst: &HashMap<TypeVar, Type>) -> Type {
        match ty {
            Type::TVar(v) => subst.get(v).cloned().unwrap_or_else(|| ty.clone()),
            Type::TArrow(t1, t2) => Type::TArrow(
                Box::new(self.instantiate_with_subst(t1, subst)),
                Box::new(self.instantiate_with_subst(t2, subst)),
            ),
            Type::TForall(vars, t) => Type::TForall(
                vars.clone(),
                Box::new(self.instantiate_with_subst(t, subst)),
            ),
            _ => ty.clone(),
        }
    }

    /// Infer the type of an expression
    pub fn infer_expr(&mut self, expr: &Expr, env: &TypeEnv) -> Result<Type, TypeError> {
        match expr {
            // Literals have known types
            Expr::Number(_) => Ok(Type::TInt),
            Expr::Float(_) => Ok(Type::TFloat),
            Expr::Bool(_) => Ok(Type::TBool),
            Expr::String(_) => Ok(Type::TString),
            Expr::Unit => Ok(Type::TUnit),

            // Variable lookup
            Expr::Var(v) => {
                let scheme = env
                    .lookup(v)
                    .ok_or_else(|| TypeError::UnboundVariable(v.clone()))?;
                Ok(self.instantiate(scheme))
            }

            // Let binding with let-polymorphism
            Expr::LetBinding {
                bound_var,
                bound_expr,
                body,
            } => {
                // Infer type of bound expression
                let bound_ty = self.infer_expr(bound_expr, env)?;

                // Generalize for let-polymorphism
                let scheme = self.generalize(bound_ty, env);

                // Check body with extended environment
                let mut new_env = env.clone();
                new_env.insert(bound_var.clone(), scheme);
                self.infer_expr(body, &new_env)
            }

            // Binary operations
            Expr::Op { op, left, right } => {
                use crate::ast::Opcode;

                let left_ty = self.infer_expr(left, env)?;
                let right_ty = self.infer_expr(right, env)?;

                // Unify the types - they must be the same
                self.unify(left_ty.clone(), right_ty.clone())?;

                match op {
                    // Arithmetic operations: return the same type as operands
                    Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                        match left_ty {
                            Type::TInt => Ok(Type::TInt),
                            Type::TFloat => Ok(Type::TFloat),
                            _ => {
                                // Default to int, but ideally we'd want to report an error here
                                // for types that don't support arithmetic operations
                                Ok(Type::TInt)
                            }
                        }
                    }

                    // Comparison operations: always return bool
                    Opcode::Equal | Opcode::NotEqual |
                    Opcode::LessThan | Opcode::GreaterThan |
                    Opcode::LessEqual | Opcode::GreaterEqual => {
                        // Operands can be int, float, or bool
                        // Result is always bool
                        Ok(Type::TBool)
                    }
                }
            }

            // Conditional expression
            Expr::If { condition, then_branch, else_branch } => {
                let cond_ty = self.infer_expr(condition, env)?;

                // Condition must be bool
                self.unify(cond_ty, Type::TBool)?;

                // Both branches must have the same type
                let then_ty = self.infer_expr(then_branch, env)?;
                let else_ty = self.infer_expr(else_branch, env)?;

                self.unify(then_ty.clone(), else_ty)?;

                Ok(then_ty)
            }

            // Function application
            Expr::Apply {
                fun_name,
                args,
                partial,
                ..
            } => {
                // Lookup function type
                let fun_scheme = env
                    .lookup(fun_name)
                    .ok_or_else(|| TypeError::UnboundVariable(fun_name.clone()))?;
                let fun_ty = self.instantiate(fun_scheme);

                if *partial {
                    self.infer_partial_apply(fun_ty, args, env)
                } else {
                    self.infer_full_apply(fun_ty, args, env)
                }
            }

            // Lambda expressions
            Expr::Lambda {
                param,
                param_type,
                body,
            } => {
                let param_ty = match param_type {
                    Some(ann) => self.annotation_to_type(ann, env)?,
                    None => Type::TVar(self.fresh_var()),
                };

                let mut new_env = env.clone();
                new_env.insert(param.clone(), TypeScheme::mono(param_ty.clone()));

                let body_ty = self.infer_expr(body, &new_env)?;
                Ok(Type::TArrow(Box::new(param_ty), Box::new(body_ty)))
            }

            Expr::PartialPlaceholder => Err(TypeError::UnknownType(
                "Partial placeholder should not appear in expressions".to_string(),
            )),
        }
    }

    /// Infer type for full function application
    fn infer_full_apply(
        &mut self,
        fun_ty: Type,
        args: &[Expr],
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let fun_ty = self.apply_substitution(&fun_ty);

        // Unpack function type to get parameter types
        let mut param_types = Vec::new();
        let mut current = fun_ty;

        while let Type::TArrow(param, rest) = current {
            param_types.push(*param);
            current = *rest;
        }

        // Check argument count
        if args.len() > param_types.len() {
            return Err(TypeError::TooManyArgs);
        }
        if args.len() < param_types.len() {
            return Err(TypeError::TooFewArgs);
        }

        // Check each argument
        for (arg, param_ty) in args.iter().zip(param_types.iter()) {
            let arg_ty = self.infer_expr(arg, env)?;
            self.unify(arg_ty, param_ty.clone())?;
        }

        Ok(current) // Return type
    }

    /// Infer type for partial application
    fn infer_partial_apply(
        &mut self,
        fun_ty: Type,
        args: &[Expr],
        env: &TypeEnv,
    ) -> Result<Type, TypeError> {
        let fun_ty = self.apply_substitution(&fun_ty);

        // Unpack function type
        let mut param_types = Vec::new();
        let mut current = fun_ty;

        while let Type::TArrow(param, rest) = current {
            param_types.push(*param);
            current = *rest;
        }

        // Check we have enough parameters for partial application
        if args.is_empty() {
            return Err(TypeError::TooFewArgs);
        }
        if args.len() >= param_types.len() {
            return Err(TypeError::TooManyArgs);
        }

        // Check provided arguments
        for (arg, param_ty) in args.iter().zip(param_types.iter()) {
            let arg_ty = self.infer_expr(arg, env)?;
            self.unify(arg_ty, param_ty.clone())?;
        }

        // Build remaining function type
        let remaining_params = &param_types[args.len()..];
        let mut result_ty = current; // Return type

        for param_ty in remaining_params.iter().rev() {
            result_ty = Type::TArrow(Box::new(param_ty.clone()), Box::new(result_ty));
        }

        Ok(result_ty)
    }

    /// Infer and generalize function type
    /// This handles BOTH explicit type params and automatic inference
    pub fn generalize_function(
        &mut self,
        function: &Function,
        env: &TypeEnv,
    ) -> Result<TypeScheme, TypeError> {
        // Create a new environment for type parameters
        let mut func_env = env.clone();

        // If function has explicit type params, register them
        for type_param in &function.type_params {
            func_env.add_type_param(type_param.clone());
        }

        // Infer parameter types
        let mut param_types = Vec::new();
        for (param_var, param_annotation) in &function.params {
            let param_ty = match param_annotation {
                Some(ann) => {
                    // Explicit annotation: use checking mode
                    self.annotation_to_type(ann, &func_env)?
                }
                None => {
                    // No annotation: create fresh type variable for inference
                    Type::TVar(self.fresh_var())
                }
            };

            // Add parameter to environment for body inference
            func_env.insert(param_var.clone(), TypeScheme::mono(param_ty.clone()));
            param_types.push(param_ty);
        }

        // Infer return type from body
        let body_ty = if let Some(ret_ann) = &function.return_type {
            // Explicit return type: use checking mode
            let expected_ty = self.annotation_to_type(ret_ann, &func_env)?;
            let inferred_ty = self.infer_expr(&function.body, &func_env)?;
            self.unify(inferred_ty, expected_ty.clone())?;
            expected_ty
        } else {
            // No return type annotation: infer it
            self.infer_expr(&function.body, &func_env)?
        };

        // Build function type: param1 -> param2 -> ... -> return_type
        let mut func_ty = body_ty;
        for param_ty in param_types.into_iter().rev() {
            func_ty = Type::TArrow(Box::new(param_ty), Box::new(func_ty));
        }

        // Apply current substitution
        func_ty = self.apply_substitution(&func_ty);

        // Generalize: any free type variables become quantified
        let scheme = self.generalize(func_ty, env);

        Ok(scheme)
    }

    /// Check an entire module
    pub fn check_module(&mut self, module: &Module) -> Result<TypeEnv, TypeError> {
        let mut env = TypeEnv::new();

        // First pass: collect function signatures
        for func in &module.0 {
            let scheme = self.generalize_function(func, &env)?;
            env.insert(func.name.clone(), scheme);
        }

        // Second pass: check function bodies (already done in generalize_function)
        // This ensures all types are consistent

        Ok(env)
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Opcode;

    #[test]
    fn test_infer_number() {
        let mut checker = TypeChecker::new();
        let env = TypeEnv::new();
        let expr = Expr::Number(42);

        let ty = checker.infer_expr(&expr, &env).unwrap();
        assert_eq!(ty, Type::TInt);
    }

    #[test]
    fn test_infer_bool() {
        let mut checker = TypeChecker::new();
        let env = TypeEnv::new();
        let expr = Expr::Bool(true);

        let ty = checker.infer_expr(&expr, &env).unwrap();
        assert_eq!(ty, Type::TBool);
    }

    #[test]
    fn test_infer_addition() {
        let mut checker = TypeChecker::new();
        let env = TypeEnv::new();
        let expr = Expr::Op {
            op: Opcode::Add,
            left: Box::new(Expr::Number(1)),
            right: Box::new(Expr::Number(2)),
        };

        let ty = checker.infer_expr(&expr, &env).unwrap();
        assert_eq!(ty, Type::TInt);
    }

    #[test]
    fn test_unify_same_types() {
        let mut checker = TypeChecker::new();
        assert!(checker.unify(Type::TInt, Type::TInt).is_ok());
    }

    #[test]
    fn test_unify_type_variable() {
        let mut checker = TypeChecker::new();
        let var = checker.fresh_var();
        assert!(checker.unify(Type::TVar(var.clone()), Type::TInt).is_ok());

        let result = checker.apply_substitution(&Type::TVar(var));
        assert_eq!(result, Type::TInt);
    }

    #[test]
    fn test_unify_arrow_types() {
        let mut checker = TypeChecker::new();
        let t1 = Type::TArrow(Box::new(Type::TInt), Box::new(Type::TBool));
        let t2 = Type::TArrow(Box::new(Type::TInt), Box::new(Type::TBool));

        assert!(checker.unify(t1, t2).is_ok());
    }

    #[test]
    fn test_occurs_check() {
        let mut checker = TypeChecker::new();
        let var = checker.fresh_var();

        // Try to unify var with (var -> int), should fail
        let recursive_ty = Type::TArrow(Box::new(Type::TVar(var.clone())), Box::new(Type::TInt));

        let result = checker.unify(Type::TVar(var), recursive_ty);
        assert!(matches!(result, Err(TypeError::InfiniteType)));
    }

    #[test]
    fn test_generalize_simple() {
        let checker = TypeChecker::new();
        let env = TypeEnv::new();
        let var = TypeVar::new(1);

        let ty = Type::TVar(var.clone());
        let scheme = checker.generalize(ty, &env);

        assert_eq!(scheme.quantifiers.len(), 1);
        assert!(scheme.quantifiers.contains(&var));
    }

    #[test]
    fn test_instantiate() {
        let mut checker = TypeChecker::new();
        let var = TypeVar::new(1);

        let scheme = TypeScheme {
            quantifiers: vec![var.clone()],
            ty: Type::TVar(var),
        };

        let instance1 = checker.instantiate(&scheme);
        let instance2 = checker.instantiate(&scheme);

        // Each instantiation should create a fresh variable
        assert_ne!(instance1, instance2);
    }
}

/// Function usage patterns for currying optimization
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionUsage {
    NonCurried, // Only called fully: add(1, 2)
    Curried,    // Only partial application: add(1, *)
    Mixed,      // Both: add(1, 2) and add(1, *)
}

/// Analyzes how functions are used to optimize codegen
pub struct UsageAnalyzer {
    function_usage: HashMap<Var, FunctionUsage>,
}

impl UsageAnalyzer {
    pub fn new() -> Self {
        UsageAnalyzer {
            function_usage: HashMap::new(),
        }
    }

    /// Analyze a module and return usage patterns for each function
    pub fn analyze_module(&mut self, module: &Module) -> HashMap<Var, FunctionUsage> {
        // Analyze all function bodies
        for func in &module.0 {
            self.analyze_expr(&func.body);
        }

        self.function_usage.clone()
    }

    /// Recursively analyze expressions to find function calls
    fn analyze_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Apply {
                fun_name, partial, ..
            } => {
                // Record this usage pattern
                let current = self
                    .function_usage
                    .entry(fun_name.clone())
                    .or_insert(if *partial {
                        FunctionUsage::Curried
                    } else {
                        FunctionUsage::NonCurried
                    });

                // Update to Mixed if we see both usage patterns
                *current = match (*current, *partial) {
                    (FunctionUsage::NonCurried, true) => FunctionUsage::Mixed,
                    (FunctionUsage::Curried, false) => FunctionUsage::Mixed,
                    _ => *current,
                };
            }

            Expr::LetBinding {
                bound_expr, body, ..
            } => {
                self.analyze_expr(bound_expr);
                self.analyze_expr(body);
            }

            Expr::Op { left, right, .. } => {
                self.analyze_expr(left);
                self.analyze_expr(right);
            }

            Expr::If { condition, then_branch, else_branch } => {
                self.analyze_expr(condition);
                self.analyze_expr(then_branch);
                self.analyze_expr(else_branch);
            }

            Expr::Lambda { body, .. } => {
                self.analyze_expr(body);
            }

            // Literals and variables don't contain function calls
            _ => {}
        }
    }
}

#[cfg(test)]
mod usage_tests {
    use super::*;
    use crate::ast::Opcode;

    #[test]
    fn test_analyze_non_curried_only() {
        let mut analyzer = UsageAnalyzer::new();

        // fun add(a, b) { a + b }
        // fun main() { add(1, 2) }
        let add_func = Function {
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

        let main_func = Function {
            name: Var("main".to_string()),
            type_params: vec![],
            params: vec![],
            return_type: None,
            body: Expr::Apply {
                fun_name: Var("add".to_string()),
                type_args: vec![],
                args: vec![Expr::Number(1), Expr::Number(2)],
                partial: false,
            },
        };

        let module = Module(vec![add_func, main_func]);
        let usage = analyzer.analyze_module(&module);

        assert_eq!(
            usage.get(&Var("add".to_string())),
            Some(&FunctionUsage::NonCurried)
        );
    }

    #[test]
    fn test_analyze_curried_only() {
        let mut analyzer = UsageAnalyzer::new();

        // fun add(a, b) { a + b }
        // fun main() { add(1, *) }
        let add_func = Function {
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

        let main_func = Function {
            name: Var("main".to_string()),
            type_params: vec![],
            params: vec![],
            return_type: None,
            body: Expr::Apply {
                fun_name: Var("add".to_string()),
                type_args: vec![],
                args: vec![Expr::Number(1)],
                partial: true,
            },
        };

        let module = Module(vec![add_func, main_func]);
        let usage = analyzer.analyze_module(&module);

        assert_eq!(
            usage.get(&Var("add".to_string())),
            Some(&FunctionUsage::Curried)
        );
    }

    #[test]
    fn test_analyze_mixed_usage() {
        let mut analyzer = UsageAnalyzer::new();

        // fun add(a, b) { a + b }
        // fun main() { add(1, 2) + add(3, *) }
        let add_func = Function {
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

        let main_func = Function {
            name: Var("main".to_string()),
            type_params: vec![],
            params: vec![],
            return_type: None,
            body: Expr::Op {
                op: Opcode::Add,
                left: Box::new(Expr::Apply {
                    fun_name: Var("add".to_string()),
                    type_args: vec![],
                    args: vec![Expr::Number(1), Expr::Number(2)],
                    partial: false,
                }),
                right: Box::new(Expr::Apply {
                    fun_name: Var("add".to_string()),
                    type_args: vec![],
                    args: vec![Expr::Number(3)],
                    partial: true,
                }),
            },
        };

        let module = Module(vec![add_func, main_func]);
        let usage = analyzer.analyze_module(&module);

        assert_eq!(
            usage.get(&Var("add".to_string())),
            Some(&FunctionUsage::Mixed)
        );
    }
}

use crate::ast::{Var, TypeDef, EnumVariant};
use std::collections::{HashMap, HashSet};

/// Type representation for Hindley-Milner type system
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    // Base types
    TInt,
    TBool,
    TString,
    TUnit,
    TFloat,

    // Type variables (for inference)
    TVar(TypeVar),

    // Function types: T1 -> T2
    TArrow(Box<Type>, Box<Type>),

    // Polymorphic types: forall a. T
    TForall(Vec<TypeVar>, Box<Type>),

    // Enum/ADT types: e.g., Tree<'a>
    TEnum(String, Vec<Type>), // (type name, type arguments)
}

impl Type {
    /// Get all free type variables in this type
    pub fn free_type_vars(&self) -> HashSet<TypeVar> {
        match self {
            Type::TInt | Type::TBool | Type::TString | Type::TUnit | Type::TFloat => HashSet::new(),
            Type::TVar(v) => {
                let mut set = HashSet::new();
                set.insert(v.clone());
                set
            }
            Type::TArrow(t1, t2) => {
                let mut set = t1.free_type_vars();
                set.extend(t2.free_type_vars());
                set
            }
            Type::TForall(quantifiers, ty) => {
                let mut set = ty.free_type_vars();
                for q in quantifiers {
                    set.remove(q);
                }
                set
            }
            Type::TEnum(_, args) => {
                let mut set = HashSet::new();
                for arg in args {
                    set.extend(arg.free_type_vars());
                }
                set
            }
        }
    }

    /// Pretty print a type
    pub fn display(&self) -> String {
        match self {
            Type::TInt => "int".to_string(),
            Type::TBool => "bool".to_string(),
            Type::TString => "string".to_string(),
            Type::TUnit => "unit".to_string(),
            Type::TFloat => "float".to_string(),
            Type::TVar(v) => v.display(),
            Type::TArrow(t1, t2) => {
                let t1_str = match &**t1 {
                    Type::TArrow(_, _) => format!("({})", t1.display()),
                    _ => t1.display(),
                };
                format!("{} -> {}", t1_str, t2.display())
            }
            Type::TForall(quantifiers, ty) => {
                let vars: Vec<_> = quantifiers.iter().map(|v| v.display()).collect();
                format!("forall {}. {}", vars.join(" "), ty.display())
            }
            Type::TEnum(name, args) => {
                if args.is_empty() {
                    name.clone()
                } else {
                    let args_str: Vec<_> = args.iter().map(|t| t.display()).collect();
                    format!("{}<{}>", name, args_str.join(", "))
                }
            }
        }
    }
}

/// Type variable for inference
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub id: usize,
    pub name: Option<String>, // For pretty printing
}

impl TypeVar {
    pub fn new(id: usize) -> Self {
        TypeVar { id, name: None }
    }

    pub fn with_name(id: usize, name: String) -> Self {
        TypeVar {
            id,
            name: Some(name),
        }
    }

    pub fn display(&self) -> String {
        match &self.name {
            Some(name) => format!("'{}", name),
            None => format!("'t{}", self.id),
        }
    }
}

/// Type scheme for let-polymorphism
/// Represents: forall quantifiers. ty
#[derive(Debug, Clone, PartialEq)]
pub struct TypeScheme {
    pub quantifiers: Vec<TypeVar>,
    pub ty: Type,
}

impl TypeScheme {
    /// Create a monomorphic type scheme (no quantifiers)
    pub fn mono(ty: Type) -> Self {
        TypeScheme {
            quantifiers: Vec::new(),
            ty,
        }
    }

    /// Get free type variables (not bound by quantifiers)
    pub fn free_type_vars(&self) -> HashSet<TypeVar> {
        let mut set = self.ty.free_type_vars();
        for q in &self.quantifiers {
            set.remove(q);
        }
        set
    }

    /// Display the type scheme with quantifiers
    pub fn display(&self) -> String {
        if self.quantifiers.is_empty() {
            self.ty.display()
        } else {
            let vars: Vec<_> = self.quantifiers.iter().map(|v| v.display()).collect();
            format!("forall {}. {}", vars.join(" "), self.ty.display())
        }
    }
}

/// Information about an enum variant constructor
#[derive(Debug, Clone)]
pub struct VariantInfo {
    pub type_name: String,
    pub type_params: Vec<String>,
    pub field_types: Vec<crate::ast::TypeAnnotation>,
}

/// Type environment for type checking
#[derive(Debug, Clone)]
pub struct TypeEnv {
    bindings: HashMap<Var, TypeScheme>,
    type_params: HashSet<String>, // Track generic type parameters in scope
    type_defs: HashMap<String, TypeDef>, // Type definitions
    constructors: HashMap<String, VariantInfo>, // Variant constructors
}

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv {
            bindings: HashMap::new(),
            type_params: HashSet::new(),
            type_defs: HashMap::new(),
            constructors: HashMap::new(),
        }
    }

    pub fn insert(&mut self, var: Var, scheme: TypeScheme) {
        self.bindings.insert(var, scheme);
    }

    pub fn lookup(&self, var: &Var) -> Option<&TypeScheme> {
        self.bindings.get(var)
    }

    pub fn add_type_param(&mut self, param: String) {
        self.type_params.insert(param);
    }

    pub fn has_type_param(&self, param: &str) -> bool {
        self.type_params.contains(param)
    }

    pub fn add_type_def(&mut self, type_def: TypeDef) {
        let type_name = type_def.name.clone();
        let type_params = type_def.type_params.clone();

        // Register each variant as a constructor
        for variant in &type_def.variants {
            self.constructors.insert(
                variant.name.clone(),
                VariantInfo {
                    type_name: type_name.clone(),
                    type_params: type_params.clone(),
                    field_types: variant.fields.clone(),
                },
            );
        }

        self.type_defs.insert(type_name, type_def);
    }

    pub fn lookup_constructor(&self, name: &str) -> Option<&VariantInfo> {
        self.constructors.get(name)
    }

    pub fn lookup_type_def(&self, name: &str) -> Option<&TypeDef> {
        self.type_defs.get(name)
    }

    /// Get all free type variables in the environment
    pub fn free_type_vars(&self) -> HashSet<TypeVar> {
        let mut set = HashSet::new();
        for scheme in self.bindings.values() {
            set.extend(scheme.free_type_vars());
        }
        set
    }
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_display() {
        let int_ty = Type::TInt;
        assert_eq!(int_ty.display(), "int");

        let var_ty = Type::TVar(TypeVar::with_name(1, "a".to_string()));
        assert_eq!(var_ty.display(), "'a");

        let fn_ty = Type::TArrow(Box::new(Type::TInt), Box::new(Type::TBool));
        assert_eq!(fn_ty.display(), "int -> bool");
    }

    #[test]
    fn test_free_type_vars() {
        let var1 = TypeVar::new(1);
        let var2 = TypeVar::new(2);

        let ty = Type::TArrow(
            Box::new(Type::TVar(var1.clone())),
            Box::new(Type::TVar(var2.clone())),
        );

        let free_vars = ty.free_type_vars();
        assert_eq!(free_vars.len(), 2);
        assert!(free_vars.contains(&var1));
        assert!(free_vars.contains(&var2));
    }

    #[test]
    fn test_forall_free_vars() {
        let var1 = TypeVar::new(1);
        let var2 = TypeVar::new(2);

        // forall 1. 1 -> 2
        let ty = Type::TForall(
            vec![var1.clone()],
            Box::new(Type::TArrow(
                Box::new(Type::TVar(var1.clone())),
                Box::new(Type::TVar(var2.clone())),
            )),
        );

        let free_vars = ty.free_type_vars();
        // Only var2 is free
        assert_eq!(free_vars.len(), 1);
        assert!(free_vars.contains(&var2));
    }

    #[test]
    fn test_type_scheme_mono() {
        let scheme = TypeScheme::mono(Type::TInt);
        assert_eq!(scheme.quantifiers.len(), 0);
        assert_eq!(scheme.ty, Type::TInt);
    }

    #[test]
    fn test_type_env() {
        let mut env = TypeEnv::new();
        let var = Var("x".to_string());
        let scheme = TypeScheme::mono(Type::TInt);

        env.insert(var.clone(), scheme.clone());
        assert_eq!(env.lookup(&var), Some(&scheme));
    }

    #[test]
    fn test_type_env_type_params() {
        let mut env = TypeEnv::new();
        env.add_type_param("a".to_string());
        assert!(env.has_type_param("a"));
        assert!(!env.has_type_param("b"));
    }
}

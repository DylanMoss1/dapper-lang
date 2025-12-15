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

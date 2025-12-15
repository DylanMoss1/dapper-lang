use crate::ast::{Expr, Function, Module, Opcode, Var};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module as LLVM_Module;
use inkwell::values::{
    AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue,
};
use inkwell::OptimizationLevel;
use std::collections::HashMap;

struct Compiler<'ctx> {
    context: &'ctx Context,
    llvm_module: LLVM_Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,

    variable_memory_alloc: HashMap<&'ctx Var, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    /// Returns the `FunctionValue` representing the function being compiled.
    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.context.f64_type(), name).unwrap()
    }

    pub fn build_load(&self, ptr: PointerValue<'ctx>, name: &str) -> BasicValueEnum<'ctx> {
        self.builder
            .build_load(self.context.i32_type(), ptr, name)
            .unwrap()
    }

    fn compile_expr(&mut self, expr: &'ctx Expr) -> Result<AnyValueEnum<'ctx>, &'static str> {
        match expr {
            Expr::Number(num_i32) => Ok(self
                .context
                .i32_type()
                .const_int(*num_i32 as u64, false)
                .as_any_value_enum()),
            Expr::LetBinding {
                bound_var,
                bound_expr,
                body,
            } => {
                let var_name = &bound_var.0;
                let compiled_body = self.compile_expr(bound_expr).unwrap();

                let alloca = self.create_entry_block_alloca(var_name);

                self.builder
                    .build_store(alloca, compiled_body.into_int_value())
                    .unwrap();

                self.variable_memory_alloc.insert(bound_var, alloca);
                self.compile_expr(body)
            }
            Expr::Var(var) => match self.variable_memory_alloc.get(var) {
                Some(var_ptr) => Ok(self.build_load(*var_ptr, &var.0).as_any_value_enum()),
                None => Err("Could not find a matching variable"),
            },
            Expr::Op { op, left, right } => {
                let lhs = self.compile_expr(left).map(AnyValueEnum::into_int_value)?;
                let rhs = self.compile_expr(right).map(AnyValueEnum::into_int_value)?;

                match op {
                    Opcode::Add => Ok(self
                        .builder
                        .build_int_add(lhs, rhs, "tmpadd")
                        .unwrap()
                        .as_any_value_enum()),
                    Opcode::Sub => Ok(self
                        .builder
                        .build_int_sub(lhs, rhs, "tmpsub")
                        .unwrap()
                        .as_any_value_enum()),
                    Opcode::Mul => Ok(self
                        .builder
                        .build_int_mul(lhs, rhs, "tmpmul")
                        .unwrap()
                        .as_any_value_enum()),
                    Opcode::Div => Ok(self
                        .builder
                        .build_int_unsigned_div(lhs, rhs, "tmpdiv")
                        .unwrap()
                        .as_any_value_enum()),
                }
            }
            Expr::Apply { fun_name, args } => {
                let fun = self.llvm_module.get_function(&fun_name.0).unwrap();
                let compiled_args = args
                    .iter()
                    .map(|arg| self.compile_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                let llvm_args: Vec<BasicMetadataValueEnum> = compiled_args
                    .into_iter()
                    .map(|arg| BasicMetadataValueEnum::IntValue(arg.into_int_value()))
                    .collect();

                let return_value = self
                    .builder
                    .build_call(fun, llvm_args.as_slice(), &format!("{}_call", fun_name.0))
                    .unwrap();

                Ok(return_value.as_any_value_enum())
            }
        }
    }

    fn compile_function(&mut self, function: &'ctx Function) -> Result<(), &'static str> {
        let Function {
            name: fun_name,
            params,
            body,
        } = function;

        let i64_type = self.context.i64_type();
        let fun_type = i64_type.fn_type(&[], false);
        let function = self.llvm_module.add_function(&fun_name.0, fun_type, None);
        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);

        let compiled_body = self.compile_expr(body).unwrap();

        let _ = self
            .builder
            .build_return(Some(&compiled_body.into_int_value()));

        Ok(())
    }

    fn compile_module(&mut self, module: &'ctx Module) -> Result<(), &'static str> {
        for function in &module.0 {
            self.compile_function(function)?;
        }

        Ok(())
    }
}

pub fn codegen_program(module: &Module) {
    let context = Context::create();
    let llvm_module = context.create_module("module");
    let builder = context.create_builder();
    let execution_engine = llvm_module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let mut codegen = Compiler {
        context: &context,
        llvm_module,
        builder,
        execution_engine,
        fn_value_opt: None,
        variable_memory_alloc: HashMap::new(),
    };

    codegen.compile_module(module).unwrap();

    type MainFn = unsafe extern "C" fn() -> i64;

    let main_fn: JitFunction<MainFn> = unsafe {
        codegen
            .execution_engine
            .get_function("main")
            .expect("Failed to get JIT function")
    };

    let result = unsafe { main_fn.call() };

    println!("Result = {}", result);
}

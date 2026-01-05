use crate::ast::{Expr, Function, Module, Opcode, Var};
use crate::typechecker::{FunctionUsage, TypeChecker};
use crate::types::{Type, TypeEnv, TypeScheme};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module as LLVM_Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
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

    // Type information from type checker
    type_env: TypeEnv,

    // Usage information for currying optimization
    usage_info: HashMap<Var, FunctionUsage>,

    // Track instantiated generic functions for future monomorphization
    // Key: (function_name, type_args_signature), Value: generated LLVM function
    #[allow(dead_code)]
    generic_instances: HashMap<(String, Vec<String>), FunctionValue<'ctx>>,

    // Counter for generating unique lambda names
    lambda_counter: usize,

    // Type checker for on-the-fly type inference
    type_checker: TypeChecker,

    // Track types of variables for type-aware compilation
    variable_types: HashMap<Var, Type>,
}

impl<'ctx> Compiler<'ctx> {
    /// Returns the `FunctionValue` representing the function being compiled.
    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    /// Infer the type of an expression using the type checker
    fn infer_type(&mut self, expr: &Expr) -> Type {
        // Create a local type environment with current variable types
        let mut local_env = self.type_env.clone();
        for (var, ty) in &self.variable_types {
            local_env.insert(var.clone(), TypeScheme::mono(ty.clone()));
        }

        // Infer type, defaulting to TInt if inference fails
        self.type_checker
            .infer_expr(expr, &local_env)
            .unwrap_or(Type::TInt)
    }

    /// Convert a Type to an LLVM BasicTypeEnum
    fn type_to_llvm(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::TInt => self.context.i32_type().into(),
            Type::TFloat => self.context.f64_type().into(),
            Type::TBool => self.context.bool_type().into(),
            Type::TString => self
                .context
                .i8_type()
                .ptr_type(inkwell::AddressSpace::default())
                .into(),
            Type::TUnit => self.context.i8_type().into(), // Unit as i8
            Type::TVar(_) => self.context.i32_type().into(), // Default to i32 for type variables
            Type::TArrow(_, _) => {
                // Implement proper closure types using function pointers
                // Extract parameter types and return type from the arrow chain
                let (param_types, return_type) = self.extract_function_signature(ty);

                // Convert parameter types to LLVM types
                let param_llvm_types: Vec<BasicMetadataTypeEnum> = param_types
                    .iter()
                    .map(|pt| self.type_to_llvm(pt).into())
                    .collect();

                // Convert return type to LLVM type
                let return_llvm_type = self.type_to_llvm(&return_type);

                // Create function type and return it as a pointer
                let fn_type = return_llvm_type.fn_type(&param_llvm_types, false);
                fn_type.ptr_type(inkwell::AddressSpace::default()).into()
            }
            Type::TForall(_, inner) => self.type_to_llvm(inner), // Look through forall
        }
    }

    /// Extract parameter types and return type from a function type (TArrow chain)
    fn extract_function_signature(&self, ty: &Type) -> (Vec<Type>, Type) {
        let mut param_types = Vec::new();
        let mut current = ty.clone();

        // Unpack the arrow type chain to get parameter types
        while let Type::TArrow(param, rest) = current {
            param_types.push(*param);
            current = *rest;
        }

        // The final type is the return type
        (param_types, current)
    }

    fn create_entry_block_alloca(&self, name: &str, ty: &Type) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        let llvm_ty = self.type_to_llvm(ty);
        builder.build_alloca(llvm_ty, name).unwrap()
    }

    pub fn build_load(&self, ptr: PointerValue<'ctx>, ty: &Type, name: &str) -> BasicValueEnum<'ctx> {
        let llvm_ty = self.type_to_llvm(ty);
        self.builder
            .build_load(llvm_ty, ptr, name)
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

                // Infer the type of the bound expression
                let bound_type = self.infer_type(bound_expr);
                let compiled_body = self.compile_expr(bound_expr).unwrap();

                let alloca = self.create_entry_block_alloca(var_name, &bound_type);

                // Store the value based on its type
                match bound_type {
                    Type::TInt => {
                        self.builder
                            .build_store(alloca, compiled_body.into_int_value())
                            .unwrap();
                    }
                    Type::TFloat => {
                        self.builder
                            .build_store(alloca, compiled_body.into_float_value())
                            .unwrap();
                    }
                    Type::TBool => {
                        self.builder
                            .build_store(alloca, compiled_body.into_int_value())
                            .unwrap();
                    }
                    Type::TString => {
                        self.builder
                            .build_store(alloca, compiled_body.into_pointer_value())
                            .unwrap();
                    }
                    Type::TUnit => {
                        self.builder
                            .build_store(alloca, compiled_body.into_int_value())
                            .unwrap();
                    }
                    Type::TArrow(_, _) => {
                        // Proper function pointer storage
                        // Function values are represented as pointers, so we can store them directly
                        if compiled_body.is_function_value() {
                            // Convert function to pointer value
                            let fn_ptr = compiled_body.into_function_value().as_global_value().as_pointer_value();
                            self.builder.build_store(alloca, fn_ptr).unwrap();
                        } else if compiled_body.is_pointer_value() {
                            // Already a pointer (e.g., from a variable lookup)
                            self.builder.build_store(alloca, compiled_body.into_pointer_value()).unwrap();
                        } else {
                            // Fallback - shouldn't normally reach here
                            self.builder.build_store(alloca, compiled_body.into_int_value()).unwrap();
                        }
                    }
                    _ => {
                        self.builder
                            .build_store(alloca, compiled_body.into_int_value())
                            .unwrap();
                    }
                }

                self.variable_memory_alloc.insert(bound_var, alloca);
                self.variable_types.insert(bound_var.clone(), bound_type);

                self.compile_expr(body)
            }
            Expr::Var(var) => match self.variable_memory_alloc.get(var) {
                Some(var_ptr) => {
                    let var_type = self.variable_types.get(var).cloned().unwrap_or(Type::TInt);
                    Ok(self.build_load(*var_ptr, &var_type, &var.0).as_any_value_enum())
                }
                None => Err("Could not find a matching variable"),
            },
            Expr::Op { op, left, right } => {
                use inkwell::FloatPredicate;
                use inkwell::IntPredicate;

                // Infer the type of the left operand to determine if we're doing int or float operations
                let left_type = self.infer_type(left);

                match op {
                    // Arithmetic operations
                    Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                        match left_type {
                            Type::TFloat => {
                                // Float operations
                                let lhs = self.compile_expr(left).map(AnyValueEnum::into_float_value)?;
                                let rhs = self.compile_expr(right).map(AnyValueEnum::into_float_value)?;

                                match op {
                                    Opcode::Add => Ok(self
                                        .builder
                                        .build_float_add(lhs, rhs, "tmpfadd")
                                        .unwrap()
                                        .as_any_value_enum()),
                                    Opcode::Sub => Ok(self
                                        .builder
                                        .build_float_sub(lhs, rhs, "tmpfsub")
                                        .unwrap()
                                        .as_any_value_enum()),
                                    Opcode::Mul => Ok(self
                                        .builder
                                        .build_float_mul(lhs, rhs, "tmpfmul")
                                        .unwrap()
                                        .as_any_value_enum()),
                                    Opcode::Div => Ok(self
                                        .builder
                                        .build_float_div(lhs, rhs, "tmpfdiv")
                                        .unwrap()
                                        .as_any_value_enum()),
                                    _ => unreachable!(),
                                }
                            }
                            _ => {
                                // Integer operations (default)
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
                                    _ => unreachable!(),
                                }
                            }
                        }
                    }

                    // Comparison operations
                    Opcode::Equal | Opcode::NotEqual | Opcode::LessThan |
                    Opcode::GreaterThan | Opcode::LessEqual | Opcode::GreaterEqual => {
                        match left_type {
                            Type::TFloat => {
                                // Float comparisons
                                let lhs = self.compile_expr(left).map(AnyValueEnum::into_float_value)?;
                                let rhs = self.compile_expr(right).map(AnyValueEnum::into_float_value)?;

                                let predicate = match op {
                                    Opcode::Equal => FloatPredicate::OEQ,
                                    Opcode::NotEqual => FloatPredicate::ONE,
                                    Opcode::LessThan => FloatPredicate::OLT,
                                    Opcode::GreaterThan => FloatPredicate::OGT,
                                    Opcode::LessEqual => FloatPredicate::OLE,
                                    Opcode::GreaterEqual => FloatPredicate::OGE,
                                    _ => unreachable!(),
                                };

                                Ok(self
                                    .builder
                                    .build_float_compare(predicate, lhs, rhs, "tmpcmp")
                                    .unwrap()
                                    .as_any_value_enum())
                            }
                            _ => {
                                // Integer/Bool comparisons
                                let lhs = self.compile_expr(left).map(AnyValueEnum::into_int_value)?;
                                let rhs = self.compile_expr(right).map(AnyValueEnum::into_int_value)?;

                                let predicate = match op {
                                    Opcode::Equal => IntPredicate::EQ,
                                    Opcode::NotEqual => IntPredicate::NE,
                                    Opcode::LessThan => IntPredicate::SLT,
                                    Opcode::GreaterThan => IntPredicate::SGT,
                                    Opcode::LessEqual => IntPredicate::SLE,
                                    Opcode::GreaterEqual => IntPredicate::SGE,
                                    _ => unreachable!(),
                                };

                                Ok(self
                                    .builder
                                    .build_int_compare(predicate, lhs, rhs, "tmpcmp")
                                    .unwrap()
                                    .as_any_value_enum())
                            }
                        }
                    }
                }
            }

            Expr::If { condition, then_branch, else_branch } => {
                // Compile the condition
                let cond_val = self.compile_expr(condition)?;
                let cond = cond_val.into_int_value();

                // Get the current function
                let function = self.fn_value();

                // Create basic blocks
                let then_block = self.context.append_basic_block(function, "then");
                let else_block = self.context.append_basic_block(function, "else");
                let merge_block = self.context.append_basic_block(function, "ifcont");

                // Build the conditional branch
                self.builder
                    .build_conditional_branch(cond, then_block, else_block)
                    .unwrap();

                // Compile the then branch
                self.builder.position_at_end(then_block);
                let then_val = self.compile_expr(then_branch)?;
                self.builder.build_unconditional_branch(merge_block).unwrap();
                let then_end_block = self.builder.get_insert_block().unwrap();

                // Compile the else branch
                self.builder.position_at_end(else_block);
                let else_val = self.compile_expr(else_branch)?;
                self.builder.build_unconditional_branch(merge_block).unwrap();
                let else_end_block = self.builder.get_insert_block().unwrap();

                // Build the merge block with phi node
                self.builder.position_at_end(merge_block);

                // Infer the result type
                let result_type = self.infer_type(then_branch);

                // Build phi node based on the result type
                let phi = match result_type {
                    Type::TFloat => {
                        let phi = self.builder.build_phi(self.context.f64_type(), "iftmp").unwrap();
                        phi.add_incoming(&[
                            (&then_val.into_float_value(), then_end_block),
                            (&else_val.into_float_value(), else_end_block),
                        ]);
                        phi.as_any_value_enum()
                    }
                    Type::TString => {
                        let phi = self.builder.build_phi(
                            self.context.i8_type().ptr_type(inkwell::AddressSpace::default()),
                            "iftmp"
                        ).unwrap();
                        phi.add_incoming(&[
                            (&then_val.into_pointer_value(), then_end_block),
                            (&else_val.into_pointer_value(), else_end_block),
                        ]);
                        phi.as_any_value_enum()
                    }
                    _ => {
                        // Int, Bool, Unit, or default
                        let phi = self.builder.build_phi(self.context.i32_type(), "iftmp").unwrap();
                        phi.add_incoming(&[
                            (&then_val.into_int_value(), then_end_block),
                            (&else_val.into_int_value(), else_end_block),
                        ]);
                        phi.as_any_value_enum()
                    }
                };

                Ok(phi)
            }
            Expr::Apply { fun_name, type_args, args, partial } => {
                // Future: if type_args is not empty, look up or create specialized version
                // For now, just ignore type_args and compile normally
                let _ = type_args; // Silence unused warning

                // Handle partial application by creating a closure
                if *partial {
                    // Find which arguments are placeholders
                    let placeholder_indices: Vec<usize> = args
                        .iter()
                        .enumerate()
                        .filter_map(|(i, arg)| {
                            if matches!(arg, Expr::PartialPlaceholder) {
                                Some(i)
                            } else {
                                None
                            }
                        })
                        .collect();

                    if placeholder_indices.is_empty() {
                        return Err("Partial application requires at least one placeholder");
                    }

                    // Get the function's type to infer parameter types
                    let func_scheme = self.type_env.lookup(fun_name)
                        .ok_or("Function not found in type environment")?;
                    let func_type = self.type_checker.apply_substitution(&func_scheme.ty);

                    // Extract parameter types from the function type
                    let (param_types, return_type) = self.extract_function_signature(&func_type);

                    // Generate a unique name for the partial application closure
                    let closure_name = format!("partial_{}_{}", fun_name.0, self.lambda_counter);
                    self.lambda_counter += 1;

                    // Get types for the placeholder parameters
                    let closure_param_types: Vec<Type> = placeholder_indices
                        .iter()
                        .map(|&i| param_types.get(i).cloned().unwrap_or(Type::TInt))
                        .collect();

                    // Convert to LLVM types
                    let closure_param_llvm_types: Vec<BasicMetadataTypeEnum> = closure_param_types
                        .iter()
                        .map(|ty| self.type_to_llvm(ty).into())
                        .collect();

                    let return_llvm_type = self.type_to_llvm(&return_type);

                    // Create closure function
                    let closure_fn_type = return_llvm_type.fn_type(&closure_param_llvm_types, false);
                    let closure_fn = self.llvm_module.add_function(&closure_name, closure_fn_type, None);
                    let closure_entry = self.context.append_basic_block(closure_fn, "entry");

                    // Save current context
                    let saved_fn = self.fn_value_opt;
                    let saved_vars = self.variable_memory_alloc.clone();
                    let saved_var_types = self.variable_types.clone();

                    // Set up closure context
                    self.fn_value_opt = Some(closure_fn);
                    self.builder.position_at_end(closure_entry);

                    // Compile the non-placeholder arguments (these are captured)
                    let mut captured_values = Vec::new();
                    for (i, arg) in args.iter().enumerate() {
                        if !matches!(arg, Expr::PartialPlaceholder) {
                            captured_values.push((i, self.compile_expr(arg)?));
                        }
                    }

                    // Restore variable context for accessing captured values
                    self.variable_memory_alloc = saved_vars.clone();
                    self.variable_types = saved_var_types.clone();

                    // Build the call to the original function with all arguments
                    let original_fn = self.llvm_module.get_function(&fun_name.0).unwrap();
                    let mut call_args: Vec<BasicMetadataValueEnum> = Vec::new();

                    let mut placeholder_param_idx = 0;
                    for (i, arg) in args.iter().enumerate() {
                        if matches!(arg, Expr::PartialPlaceholder) {
                            // Use the closure parameter
                            let param = closure_fn.get_nth_param(placeholder_param_idx as u32).unwrap();
                            let param_type = &closure_param_types[placeholder_param_idx];
                            call_args.push(match param_type {
                                Type::TFloat => BasicMetadataValueEnum::FloatValue(param.into_float_value()),
                                Type::TString => BasicMetadataValueEnum::PointerValue(param.into_pointer_value()),
                                _ => BasicMetadataValueEnum::IntValue(param.into_int_value()),
                            });
                            placeholder_param_idx += 1;
                        } else {
                            // Use the captured value
                            let (_, captured_val) = captured_values
                                .iter()
                                .find(|(idx, _)| *idx == i)
                                .unwrap();
                            let arg_type = &param_types[i];
                            call_args.push(match arg_type {
                                Type::TFloat => BasicMetadataValueEnum::FloatValue(captured_val.into_float_value()),
                                Type::TString => BasicMetadataValueEnum::PointerValue(captured_val.into_pointer_value()),
                                _ => BasicMetadataValueEnum::IntValue(captured_val.into_int_value()),
                            });
                        }
                    }

                    let call_site = self
                        .builder
                        .build_call(original_fn, &call_args, "partial_call")
                        .unwrap();

                    // Extract the return value and build return
                    match call_site.try_as_basic_value() {
                        inkwell::values::ValueKind::Basic(basic_val) => {
                            match basic_val {
                                inkwell::values::BasicValueEnum::IntValue(v) => {
                                    self.builder.build_return(Some(&v)).unwrap();
                                }
                                inkwell::values::BasicValueEnum::FloatValue(v) => {
                                    self.builder.build_return(Some(&v)).unwrap();
                                }
                                inkwell::values::BasicValueEnum::PointerValue(v) => {
                                    self.builder.build_return(Some(&v)).unwrap();
                                }
                                _ => {
                                    // Other basic types, try to return as-is
                                    self.builder.build_return(Some(&basic_val)).unwrap();
                                }
                            }
                        }
                        _ => {
                            // Void or other non-basic type
                            self.builder.build_return(None).unwrap();
                        }
                    }

                    // Restore original context
                    self.fn_value_opt = saved_fn;
                    self.variable_memory_alloc = saved_vars;
                    self.variable_types = saved_var_types;

                    if let Some(current_fn) = saved_fn {
                        if let Some(current_block) = current_fn.get_last_basic_block() {
                            self.builder.position_at_end(current_block);
                        }
                    }

                    // Return the closure function as a value
                    return Ok(closure_fn.as_global_value().as_any_value_enum());
                }

                let fun = self.llvm_module.get_function(&fun_name.0).unwrap();
                let compiled_args = args
                    .iter()
                    .map(|arg| self.compile_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                // Convert arguments to the correct type based on their inferred types
                let llvm_args: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .zip(compiled_args.iter())
                    .map(|(arg_expr, arg_val)| {
                        let arg_type = self.infer_type(arg_expr);
                        match arg_type {
                            Type::TFloat => BasicMetadataValueEnum::FloatValue(arg_val.into_float_value()),
                            Type::TString => BasicMetadataValueEnum::PointerValue(arg_val.into_pointer_value()),
                            Type::TBool | Type::TInt | Type::TUnit | _ => {
                                BasicMetadataValueEnum::IntValue(arg_val.into_int_value())
                            }
                        }
                    })
                    .collect();

                let return_value = self
                    .builder
                    .build_call(fun, llvm_args.as_slice(), &format!("{}_call", fun_name.0))
                    .unwrap();

                Ok(return_value.as_any_value_enum())
            }

            // New literal types
            Expr::Float(f) => Ok(self
                .context
                .f64_type()
                .const_float(*f)
                .as_any_value_enum()),

            Expr::Bool(b) => Ok(self
                .context
                .bool_type()
                .const_int(*b as u64, false)
                .as_any_value_enum()),

            Expr::String(s) => {
                let str_val = self.builder.build_global_string_ptr(s, "str").unwrap();
                Ok(str_val.as_any_value_enum())
            }

            Expr::Unit => Ok(self
                .context
                .i8_type()
                .const_zero()
                .as_any_value_enum()),

            Expr::Lambda { param, param_type, body } => {
                // For now, we'll implement a simplified version that generates an anonymous function
                // Full closure support with captured variables would require more complex implementation

                // Generate a unique name for the lambda function
                let lambda_name = format!("lambda_{}", self.lambda_counter);
                self.lambda_counter += 1;

                // Infer the lambda's type to determine parameter and return types
                let lambda_expr_type = self.infer_type(expr);

                let (param_ty, return_ty) = match lambda_expr_type {
                    Type::TArrow(p, r) => (*p, *r),
                    _ => (Type::TInt, Type::TInt), // Default fallback
                };

                // Convert parameter type to LLVM type
                let param_llvm_type = self.type_to_llvm(&param_ty);
                let return_llvm_type = self.type_to_llvm(&return_ty);

                // Create lambda function signature
                let lambda_type = return_llvm_type.fn_type(&[param_llvm_type.into()], false);
                let lambda_fn = self.llvm_module.add_function(&lambda_name, lambda_type, None);
                let lambda_entry = self.context.append_basic_block(lambda_fn, "entry");

                // Save current function context
                let saved_fn = self.fn_value_opt;
                let saved_vars = self.variable_memory_alloc.clone();
                let saved_var_types = self.variable_types.clone();

                // Set up new function context
                self.fn_value_opt = Some(lambda_fn);
                // Note: We keep the saved_vars to support captured variables
                // This is a simplified closure implementation - proper closures would
                // need to pass captured variables as part of a closure struct

                // Position builder at lambda entry
                self.builder.position_at_end(lambda_entry);

                // Register lambda parameter
                let llvm_param = lambda_fn.get_nth_param(0).unwrap();
                let param_alloca = self.create_entry_block_alloca(&param.0, &param_ty);

                // Store based on type
                match param_ty {
                    Type::TInt | Type::TBool | Type::TUnit => {
                        self.builder.build_store(param_alloca, llvm_param.into_int_value()).unwrap();
                    }
                    Type::TFloat => {
                        self.builder.build_store(param_alloca, llvm_param.into_float_value()).unwrap();
                    }
                    Type::TString => {
                        self.builder.build_store(param_alloca, llvm_param.into_pointer_value()).unwrap();
                    }
                    _ => {
                        self.builder.build_store(param_alloca, llvm_param.into_int_value()).unwrap();
                    }
                }

                self.variable_memory_alloc.insert(param, param_alloca);
                self.variable_types.insert(param.clone(), param_ty);

                // Compile lambda body
                let lambda_body_val = self.compile_expr(body)?;

                // Return based on return type
                match return_ty {
                    Type::TInt | Type::TBool | Type::TUnit => {
                        self.builder.build_return(Some(&lambda_body_val.into_int_value())).unwrap();
                    }
                    Type::TFloat => {
                        self.builder.build_return(Some(&lambda_body_val.into_float_value())).unwrap();
                    }
                    Type::TString => {
                        self.builder.build_return(Some(&lambda_body_val.into_pointer_value())).unwrap();
                    }
                    _ => {
                        self.builder.build_return(Some(&lambda_body_val.into_int_value())).unwrap();
                    }
                }

                // Restore original function context
                self.fn_value_opt = saved_fn;
                self.variable_memory_alloc = saved_vars;
                self.variable_types = saved_var_types;

                // Position builder back to original position
                if let Some(current_fn) = saved_fn {
                    if let Some(current_block) = current_fn.get_last_basic_block() {
                        self.builder.position_at_end(current_block);
                    }
                }

                // Return the function pointer as a value
                // For now, we'll represent it as an integer (function address)
                // In a full implementation, this would be a proper function pointer or closure struct
                Ok(lambda_fn.as_global_value().as_any_value_enum())
            },

            Expr::PartialPlaceholder => Err("Partial placeholder should not appear in expressions"),
        }
    }

    fn compile_function(&mut self, function: &'ctx Function) -> Result<(), &'static str> {
        let Function {
            name: fun_name,
            params,
            body,
            ..
        } = function;

        // Get the function's type scheme from the type environment
        let func_scheme = self.type_env.lookup(fun_name).ok_or("Function not found in type environment")?;
        let func_type = self.type_checker.apply_substitution(&func_scheme.ty);

        // Extract parameter types and return type from the function type
        let mut param_types_ir = Vec::new();
        let mut current = func_type;

        // Unpack the arrow type chain to get parameter types
        while let Type::TArrow(param, rest) = current {
            param_types_ir.push(*param);
            current = *rest;
        }

        let return_type = current;

        // Convert parameter types to LLVM types
        let param_types_llvm: Vec<BasicMetadataTypeEnum> = param_types_ir
            .iter()
            .map(|ty| self.type_to_llvm(ty).into())
            .collect();

        // Convert return type to LLVM type
        let return_type_llvm = self.type_to_llvm(&return_type);

        // Create function signature with inferred types
        let fun_type = return_type_llvm.fn_type(&param_types_llvm, false);
        let function_value = self.llvm_module.add_function(&fun_name.0, fun_type, None);
        let entry = self.context.append_basic_block(function_value, "entry");

        self.builder.position_at_end(entry);

        // Set fn_value_opt for this function
        self.fn_value_opt = Some(function_value);

        // Register parameters as variables
        self.variable_memory_alloc.clear(); // Clear variables from previous function
        self.variable_types.clear(); // Clear variable types from previous function

        for (i, (param_var, _param_type)) in params.iter().enumerate() {
            let param_ty = &param_types_ir[i];
            let llvm_param = function_value.get_nth_param(i as u32).unwrap();

            let alloca = self.create_entry_block_alloca(&param_var.0, param_ty);

            // Store based on type
            match param_ty {
                Type::TInt | Type::TBool | Type::TUnit => {
                    self.builder.build_store(alloca, llvm_param.into_int_value()).unwrap();
                }
                Type::TFloat => {
                    self.builder.build_store(alloca, llvm_param.into_float_value()).unwrap();
                }
                Type::TString => {
                    self.builder.build_store(alloca, llvm_param.into_pointer_value()).unwrap();
                }
                _ => {
                    self.builder.build_store(alloca, llvm_param.into_int_value()).unwrap();
                }
            }

            self.variable_memory_alloc.insert(param_var, alloca);
            self.variable_types.insert(param_var.clone(), param_ty.clone());
        }

        // Compile function body
        let compiled_body = self.compile_expr(body)?;

        // Build return based on return type
        match return_type {
            Type::TInt | Type::TBool | Type::TUnit => {
                self.builder.build_return(Some(&compiled_body.into_int_value())).unwrap();
            }
            Type::TFloat => {
                self.builder.build_return(Some(&compiled_body.into_float_value())).unwrap();
            }
            Type::TString => {
                self.builder.build_return(Some(&compiled_body.into_pointer_value())).unwrap();
            }
            _ => {
                self.builder.build_return(Some(&compiled_body.into_int_value())).unwrap();
            }
        }

        Ok(())
    }

    fn compile_module(&mut self, module: &'ctx Module) -> Result<(), &'static str> {
        for function in &module.0 {
            self.compile_function(function)?;
        }

        Ok(())
    }
}

pub fn codegen_program(
    module: &Module,
    type_env: TypeEnv,
    usage_info: HashMap<Var, FunctionUsage>,
) {
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
        type_env,
        usage_info,
        generic_instances: HashMap::new(),
        lambda_counter: 0,
        type_checker: TypeChecker::new(),
        variable_types: HashMap::new(),
    };

    codegen.compile_module(module).unwrap();

    // Determine the return type of main function to call it correctly
    let main_type = codegen.type_env.lookup(&Var("main".to_string()))
        .map(|scheme| codegen.type_checker.apply_substitution(&scheme.ty))
        .unwrap_or(Type::TInt);

    match main_type {
        Type::TInt => {
            type MainFn = unsafe extern "C" fn() -> i32;
            let main_fn: JitFunction<MainFn> = unsafe {
                codegen
                    .execution_engine
                    .get_function("main")
                    .expect("Failed to get JIT function")
            };
            let result = unsafe { main_fn.call() };
            println!("Result = {}", result);
        }
        Type::TFloat => {
            type MainFn = unsafe extern "C" fn() -> f64;
            let main_fn: JitFunction<MainFn> = unsafe {
                codegen
                    .execution_engine
                    .get_function("main")
                    .expect("Failed to get JIT function")
            };
            let result = unsafe { main_fn.call() };
            println!("Result = {}", result);
        }
        Type::TBool => {
            type MainFn = unsafe extern "C" fn() -> bool;
            let main_fn: JitFunction<MainFn> = unsafe {
                codegen
                    .execution_engine
                    .get_function("main")
                    .expect("Failed to get JIT function")
            };
            let result = unsafe { main_fn.call() };
            println!("Result = {}", result);
        }
        Type::TUnit => {
            type MainFn = unsafe extern "C" fn() -> u8;
            let main_fn: JitFunction<MainFn> = unsafe {
                codegen
                    .execution_engine
                    .get_function("main")
                    .expect("Failed to get JIT function")
            };
            let _ = unsafe { main_fn.call() };
            println!("Result = ()");
        }
        _ => {
            // Default to i32
            type MainFn = unsafe extern "C" fn() -> i32;
            let main_fn: JitFunction<MainFn> = unsafe {
                codegen
                    .execution_engine
                    .get_function("main")
                    .expect("Failed to get JIT function")
            };
            let result = unsafe { main_fn.call() };
            println!("Result = {}", result);
        }
    }
}

pub fn codegen_to_file(
    module: &Module,
    type_env: TypeEnv,
    usage_info: HashMap<Var, FunctionUsage>,
    output_path: &std::path::Path,
) -> Result<(), String> {
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
        type_env,
        usage_info,
        generic_instances: HashMap::new(),
        lambda_counter: 0,
        type_checker: TypeChecker::new(),
        variable_types: HashMap::new(),
    };

    codegen.compile_module(module).unwrap();

    // Write LLVM IR to file
    codegen
        .llvm_module
        .print_to_file(output_path)
        .map_err(|e| format!("Failed to write LLVM IR: {}", e))?;

    Ok(())
}

use clap::{Parser, Subcommand};
use lalrpop_util::lalrpop_mod;
use std::fs;
use std::path::PathBuf;

lalrpop_mod!(pub parser);

mod ast;
mod codegen;
mod lexer;
mod typechecker;
mod types;

use crate::lexer::{LexerBridge, Token};
use crate::parser::ModuleParser;
use crate::typechecker::{TypeChecker, UsageAnalyzer};

#[derive(Parser)]
#[command(name = "dapper")]
#[command(about = "Dapper Programming Language Compiler", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile and run a Dapper program
    Run {
        /// Path to the Dapper source file
        #[arg(value_name = "FILE")]
        file: PathBuf,
    },
    /// Compile a Dapper program (outputs LLVM IR)
    Build {
        /// Path to the Dapper source file
        #[arg(value_name = "FILE")]
        file: PathBuf,
        /// Output file path (optional)
        #[arg(short, long, value_name = "OUTPUT")]
        output: Option<PathBuf>,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run { file } => {
            if let Err(e) = run_file(&file) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
        Commands::Build { file, output } => {
            if let Err(e) = build_file(&file, output) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
    }
}

fn run_file(path: &PathBuf) -> Result<(), String> {
    let source = fs::read_to_string(path)
        .map_err(|e| format!("Failed to read file '{}': {}", path.display(), e))?;

    println!("Running: {}", path.display());
    compile_and_run(&source)?;
    Ok(())
}

fn build_file(path: &PathBuf, output: Option<PathBuf>) -> Result<(), String> {
    let source = fs::read_to_string(path)
        .map_err(|e| format!("Failed to read file '{}': {}", path.display(), e))?;

    let output_path = output.unwrap_or_else(|| {
        let mut p = path.clone();
        p.set_extension("ll");
        p
    });

    println!("Building: {}", path.display());
    compile_to_ir(&source, &output_path)?;
    println!("Output written to: {}", output_path.display());
    Ok(())
}

fn compile_and_run(source: &str) -> Result<(), String> {
    use logos::Logos;

    // Parse
    let lexer = Token::lexer(source);
    let lexer_bridge = LexerBridge { lexer };
    let parser = ModuleParser::new();

    let module = parser
        .parse(lexer_bridge)
        .map_err(|e| format!("Parse error: {:?}", e))?;

    // Type checking
    let mut type_checker = TypeChecker::new();
    let type_env = type_checker
        .check_module(&module)
        .map_err(|e| format!("Type error: {:?}", e))?;

    println!("Type checking passed ✓");
    println!("Inferred types:");
    for func in &module.functions {
        if let Some(scheme) = type_env.lookup(&func.name) {
            println!("  {}: {}", func.name.0, scheme.display());
        }
    }

    // Usage analysis
    let mut usage_analyzer = UsageAnalyzer::new();
    let usage_info = usage_analyzer.analyze_module(&module);

    // Code generation and execution
    println!("\nExecuting...");
    codegen::codegen_program(&module, type_env, usage_info);

    Ok(())
}

fn compile_to_ir(source: &str, output_path: &PathBuf) -> Result<(), String> {
    use logos::Logos;

    // Parse
    let lexer = Token::lexer(source);
    let lexer_bridge = LexerBridge { lexer };
    let parser = ModuleParser::new();

    let module = parser
        .parse(lexer_bridge)
        .map_err(|e| format!("Parse error: {:?}", e))?;

    // Type checking
    let mut type_checker = TypeChecker::new();
    let type_env = type_checker
        .check_module(&module)
        .map_err(|e| format!("Type error: {:?}", e))?;

    println!("Type checking passed ✓");
    println!("Inferred types:");
    for func in &module.functions {
        if let Some(scheme) = type_env.lookup(&func.name) {
            println!("  {}: {}", func.name.0, scheme.display());
        }
    }

    // Usage analysis
    let mut usage_analyzer = UsageAnalyzer::new();
    let usage_info = usage_analyzer.analyze_module(&module);

    // Generate LLVM IR
    codegen::codegen_to_file(&module, type_env, usage_info, output_path)?;

    Ok(())
}

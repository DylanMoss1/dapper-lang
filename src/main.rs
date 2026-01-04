use lalrpop_util::lalrpop_mod;
use logos::Logos;
lalrpop_mod!(pub parser);

mod ast;
mod codegen;
mod lexer;

use crate::lexer::{LexerBridge, Token};
use crate::parser::ModuleParser;

fn main() {
    let input = "fun x(z) { z + 2 } fun main() { x(1) + 1 }";

    let lexer = Token::lexer(input);
    let lexer_bridge = LexerBridge { lexer };
    let parser = ModuleParser::new();

    let module = parser.parse(lexer_bridge).unwrap();
    codegen::codegen_program(&module);
}

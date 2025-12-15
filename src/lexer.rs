use logos::{Lexer, Logos};

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    // Operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,

    // Parens
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,

    // Variables bindings
    #[token("let")]
    Let,
    #[token("=")]
    Assign,
    #[token("in")]
    In,

    // Functions
    #[token("fun")]
    Function,

    // Identifiers
    #[regex(r"[a-zA-Z][a-zA-Z0-9_']*", |lex| lex.slice().to_string(), priority = 1)]
    Identifier(String),

    // Numbers
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i32>().unwrap_or(0))]
    Number(i32),

    // Skip whitespace
    #[regex(r"[ \t\n\r]+", logos::skip)]
    // Return an error if none of the previous cases match
    Error,
}

pub struct LexerBridge<'source> {
    pub lexer: Lexer<'source, Token>,
}

impl<'source> Iterator for LexerBridge<'source> {
    type Item = (usize, Token, usize);
    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next().map(|token| {
            let span = self.lexer.span();
            (span.start, token.unwrap(), span.end)
        })
    }
}

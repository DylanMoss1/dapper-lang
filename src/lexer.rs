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
    #[token(",")]
    Comma,

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

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize(input: &str) -> Vec<Token> {
        Token::lexer(input)
            .filter_map(|result| result.ok())
            .collect()
    }

    #[test]
    fn test_tokenize_numbers() {
        let tokens = tokenize("42 123 0");
        assert_eq!(tokens, vec![
            Token::Number(42),
            Token::Number(123),
            Token::Number(0),
        ]);
    }

    #[test]
    fn test_tokenize_identifiers() {
        let tokens = tokenize("foo bar x_123 camelCase snake_case");
        assert_eq!(tokens, vec![
            Token::Identifier("foo".to_string()),
            Token::Identifier("bar".to_string()),
            Token::Identifier("x_123".to_string()),
            Token::Identifier("camelCase".to_string()),
            Token::Identifier("snake_case".to_string()),
        ]);
    }

    #[test]
    fn test_tokenize_operators() {
        let tokens = tokenize("+ - * /");
        assert_eq!(tokens, vec![
            Token::Plus,
            Token::Minus,
            Token::Star,
            Token::Slash,
        ]);
    }

    #[test]
    fn test_tokenize_delimiters() {
        let tokens = tokenize("( ) { } ,");
        assert_eq!(tokens, vec![
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
        ]);
    }

    #[test]
    fn test_tokenize_keywords() {
        let tokens = tokenize("let in fun =");
        assert_eq!(tokens, vec![
            Token::Let,
            Token::In,
            Token::Function,
            Token::Assign,
        ]);
    }

    #[test]
    fn test_tokenize_function_with_params() {
        let tokens = tokenize("fun add(a, b) { a + b }");
        assert_eq!(tokens, vec![
            Token::Function,
            Token::Identifier("add".to_string()),
            Token::LParen,
            Token::Identifier("a".to_string()),
            Token::Comma,
            Token::Identifier("b".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Identifier("a".to_string()),
            Token::Plus,
            Token::Identifier("b".to_string()),
            Token::RBrace,
        ]);
    }

    #[test]
    fn test_tokenize_let_expression() {
        let tokens = tokenize("let x = 5 in x + 1");
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier("x".to_string()),
            Token::Assign,
            Token::Number(5),
            Token::In,
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Number(1),
        ]);
    }

    #[test]
    fn test_tokenize_complex_expression() {
        let tokens = tokenize("fun main() { 2 * 3 + 4 / 2 }");
        assert_eq!(tokens, vec![
            Token::Function,
            Token::Identifier("main".to_string()),
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::Number(2),
            Token::Star,
            Token::Number(3),
            Token::Plus,
            Token::Number(4),
            Token::Slash,
            Token::Number(2),
            Token::RBrace,
        ]);
    }

    #[test]
    fn test_tokenize_function_call() {
        let tokens = tokenize("foo(1, 2, 3)");
        assert_eq!(tokens, vec![
            Token::Identifier("foo".to_string()),
            Token::LParen,
            Token::Number(1),
            Token::Comma,
            Token::Number(2),
            Token::Comma,
            Token::Number(3),
            Token::RParen,
        ]);
    }

    #[test]
    fn test_whitespace_ignored() {
        let tokens = tokenize("   1   +   2   ");
        assert_eq!(tokens, vec![
            Token::Number(1),
            Token::Plus,
            Token::Number(2),
        ]);
    }
}

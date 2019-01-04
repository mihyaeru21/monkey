use std::fmt;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum TokenType {
    Illegal,
    EOF,

    // 識別子 + リテラル
    Ident,
    Int,

    // 演算子
    Assign,   // =
    Plus,     // +
    Minus,    // -
    Bang,     // !
    Asterisk, // *
    Slash,    // /

    Lt, // <
    Gt, // >

    Eq,    // ==
    NotEq, // !=

    // デリミタ
    Comma,
    Semicolon,

    LParen, // (
    RParen, // )
    LBrace, // {
    RBrace, // }

    // キーワード
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl TokenType {
    pub fn lookup_ident(ident: &str) -> TokenType {
        match ident {
            "fn" => TokenType::Function,
            "let" => TokenType::Let,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            _ => TokenType::Ident,
        }
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let hoge = match self {
            TokenType::Illegal => "Illegal",
            TokenType::EOF => "EOF",
            TokenType::Ident => "Identifier",
            TokenType::Int => "Integer literal",
            TokenType::Assign => "=",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Bang => "!",
            TokenType::Asterisk => "*",
            TokenType::Slash => "/",
            TokenType::Lt => "<",
            TokenType::Gt => ">",
            TokenType::Eq => "==",
            TokenType::NotEq => "!=",
            TokenType::Comma => ",",
            TokenType::Semicolon => ";",
            TokenType::LParen => "(",
            TokenType::RParen => ")",
            TokenType::LBrace => "{",
            TokenType::RBrace => "}",
            TokenType::Function => "fn",
            TokenType::Let => "let",
            TokenType::True => "true",
            TokenType::False => "false",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::Return => "return",
        };
        fmt::Display::fmt(hoge, f)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

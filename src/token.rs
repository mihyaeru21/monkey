#[derive(Debug, Eq, PartialEq)]
pub enum TokenType {
    Illegal,
    EOF,

    // 識別子 + リテラル
    Ident,
    Int,

    // 演算子
    Assign,
    Plus,

    // デリミタ
    Comma,
    Semicolon,

    LParen, // (
    RParen, // }
    LBrace, // {
    RBrace, // }

    // キーワード
    Function,
    Let,
}

impl TokenType {
    pub fn lookup_ident(ident: &str) -> TokenType {
        match ident {
            "fn" => TokenType::Function,
            "let" => TokenType::Let,
            _ => TokenType::Ident,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

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

#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

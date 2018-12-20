use crate::token::{Token, TokenType};

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: None,
        };
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        let token = match self.ch {
            Some(c @ '=') => new_token(&c, TokenType::Assign),
            Some(c @ ';') => new_token(&c, TokenType::Semicolon),
            Some(c @ '(') => new_token(&c, TokenType::LParen),
            Some(c @ ')') => new_token(&c, TokenType::RParen),
            Some(c @ ',') => new_token(&c, TokenType::Comma),
            Some(c @ '+') => new_token(&c, TokenType::Plus),
            Some(c @ '{') => new_token(&c, TokenType::LBrace),
            Some(c @ '}') => new_token(&c, TokenType::RBrace),
            _ => Token {
                token_type: TokenType::EOF,
                literal: "".to_string(),
            },
        };

        self.read_char();
        token
    }

    fn read_char(&mut self) {
        self.ch = self.input.get(self.read_position).map(|i| *i);
        self.position = self.read_position;
        self.read_position += 1;
    }
}

fn new_token<L: ToString>(literal: &L, token_type: TokenType) -> Token {
    Token {
        token_type,
        literal: literal.to_string(),
    }
}

#[test]
fn test_next_token() {
    let input = "=+(){},;";

    let tests: Vec<(TokenType, String)> = vec![
        (TokenType::Assign, "=".into()),
        (TokenType::Plus, "+".into()),
        (TokenType::LParen, "(".into()),
        (TokenType::RParen, ")".into()),
        (TokenType::LBrace, "{".into()),
        (TokenType::RBrace, "}".into()),
        (TokenType::Comma, ",".into()),
        (TokenType::Semicolon, ";".into()),
        (TokenType::EOF, "".into()),
    ];

    let mut lexer = Lexer::new(input);

    for t in tests {
        let token = lexer.next_token();
        assert_eq!(token.token_type, t.0);
        assert_eq!(token.literal, t.1);
    }

    assert_eq!(1, 1);
}

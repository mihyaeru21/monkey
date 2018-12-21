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
        self.skip_whitespace();

        let token = match self.ch {
            Some(c @ '=') => new_token(c, TokenType::Assign),
            Some(c @ ';') => new_token(c, TokenType::Semicolon),
            Some(c @ '(') => new_token(c, TokenType::LParen),
            Some(c @ ')') => new_token(c, TokenType::RParen),
            Some(c @ ',') => new_token(c, TokenType::Comma),
            Some(c @ '+') => new_token(c, TokenType::Plus),
            Some(c @ '{') => new_token(c, TokenType::LBrace),
            Some(c @ '}') => new_token(c, TokenType::RBrace),
            Some(c) => {
                if is_letter(c) {
                    // return しとかないと read_char() が余分に呼び出されてしまう
                    let literal = self.read_identifier();
                    let token_type = TokenType::lookup_ident(&literal);
                    return new_token(literal, token_type);
                } else if is_digit(c) {
                    // return しとかないと read_char() が余分に呼び出されてしまう
                    return new_token(self.read_number(), TokenType::Int);
                } else {
                    new_token(c, TokenType::Illegal)
                }
            }
            None => Token {
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

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter_opt(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].iter().collect()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while is_digit_opt(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].iter().collect()
    }

    fn skip_whitespace(&mut self) {
        while is_whitespace_opt(self.ch) {
            self.read_char();
        }
    }
}

fn new_token<L: ToString>(literal: L, token_type: TokenType) -> Token {
    Token {
        token_type,
        literal: literal.to_string(),
    }
}

fn is_letter(ch: char) -> bool {
    match ch {
        'a'...'z' | 'A'...'Z' | '_' => true,
        _ => false,
    }
}

fn is_letter_opt(ch: Option<char>) -> bool {
    match ch {
        Some(c) => is_letter(c),
        None => false,
    }
}

fn is_digit(ch: char) -> bool {
    match ch {
        '0'...'9' => true,
        _ => false,
    }
}

fn is_digit_opt(ch: Option<char>) -> bool {
    match ch {
        Some(c) => is_digit(c),
        None => false,
    }
}

fn is_whitespace(ch: char) -> bool {
    match ch {
        ' ' | '\t' | '\n' | '\r' => true,
        _ => false,
    }
}

fn is_whitespace_opt(ch: Option<char>) -> bool {
    match ch {
        Some(c) => is_whitespace(c),
        None => false,
    }
}

#[test]
fn test_next_token() {
    let input = r#"
        let five = 5;
        let ten = 10;

        let add = fn(x, y) {
          x + y;
        };

        let result = add(five, ten);
    "#;

    let tests: Vec<(TokenType, &str)> = vec![
        (TokenType::Let, "let"),
        (TokenType::Ident, "five"),
        (TokenType::Assign, "="),
        (TokenType::Int, "5"),
        (TokenType::Semicolon, ";"),
        (TokenType::Let, "let"),
        (TokenType::Ident, "ten"),
        (TokenType::Assign, "="),
        (TokenType::Int, "10"),
        (TokenType::Semicolon, ";"),
        (TokenType::Let, "let"),
        (TokenType::Ident, "add"),
        (TokenType::Assign, "="),
        (TokenType::Function, "fn"),
        (TokenType::LParen, "("),
        (TokenType::Ident, "x"),
        (TokenType::Comma, ","),
        (TokenType::Ident, "y"),
        (TokenType::RParen, ")"),
        (TokenType::LBrace, "{"),
        (TokenType::Ident, "x"),
        (TokenType::Plus, "+"),
        (TokenType::Ident, "y"),
        (TokenType::Semicolon, ";"),
        (TokenType::RBrace, "}"),
        (TokenType::Semicolon, ";"),
        (TokenType::Let, "let"),
        (TokenType::Ident, "result"),
        (TokenType::Assign, "="),
        (TokenType::Ident, "add"),
        (TokenType::LParen, "("),
        (TokenType::Ident, "five"),
        (TokenType::Comma, ","),
        (TokenType::Ident, "ten"),
        (TokenType::RParen, ")"),
        (TokenType::Semicolon, ";"),
        (TokenType::EOF, ""),
    ];

    let mut lexer = Lexer::new(input);

    for t in tests {
        let token = lexer.next_token();
        assert_eq!(token.token_type, t.0);
        assert_eq!(token.literal, t.1);
    }
}

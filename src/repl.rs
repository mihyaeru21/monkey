use crate::lexer::Lexer;
use crate::token::TokenType;
use std::io::{self, BufRead, Write};

const PROMPT: &'static str = ">> ";

pub fn start<R: BufRead, W: Write>(mut input: R, mut output: W) -> io::Result<()> {
    loop {
        write!(output, "{}", PROMPT)?;
        output.flush()?;

        let mut buf = String::new();
        let count = input.read_line(&mut buf)?;
        if count == 0 {
            return Ok(());
        }

        let mut lexer = Lexer::new(&buf);

        loop {
            let token = lexer.next_token();
            if token.token_type == TokenType::EOF {
                break;
            }
            writeln!(output, "{:?}", token)?;
        }
    }
}

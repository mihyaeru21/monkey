use crate::evaluator::eval;
use crate::lexer::Lexer;
use crate::object::Environment;
use crate::parser::Parser;
use std::cell::RefCell;
use std::io::{self, BufRead, Write};
use std::rc::Rc;

const PROMPT: &'static str = ">> ";

pub fn start<R: BufRead, W: Write>(mut input: R, mut output: W) -> io::Result<()> {
    let env = Rc::new(RefCell::new(Environment::new()));

    loop {
        write!(output, "{}", PROMPT)?;
        output.flush()?;

        let mut buf = String::new();
        let count = input.read_line(&mut buf)?;
        if count == 0 {
            return Ok(());
        }

        let mut parser = Parser::new(Lexer::new(&buf));

        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(_) => {
                print_parser_errors(&mut output, parser.get_errors())?;
                continue;
            }
        };

        match eval(&program, &env) {
            Ok(obj) => writeln!(output, "{}", obj.inspect())?,
            Err(e) => writeln!(output, "{}", e)?,
        };
    }
}

const MONKEY_FACE: &'static str = r#"           __,__
  .--.  .-"     "-.  .--.
 / .. \/  .-. .-.  \/ .. \
| |  '|  /   Y   \  |'  | |
| \   \  \ 0 | 0 /  /   / |
 \ '- ,\.-"""""""-./, -' /
  ''-' /_   ^ ^   _\ '-''
      |  \._   _./  |
      \   \ '~' /   /
       '._ '-=-' _.'
          '-----'
"#;

fn print_parser_errors<W: Write>(output: &mut W, errors: &Vec<String>) -> io::Result<()> {
    write!(output, "{}", MONKEY_FACE)?;
    writeln!(output, "Woops! We ran into some monkey business here!")?;
    writeln!(output, " parser errors:")?;
    for error in errors {
        writeln!(output, "\t{}", error)?;
    }
    Ok(())
}

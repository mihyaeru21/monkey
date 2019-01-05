mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

use std::env;
use std::io;

fn main() {
    let user = env::var("USER").unwrap_or_else(|_| "anonymous user".into());
    println!("Hello {}! This is the Monkey programming language!", user);
    println!("Feel free to type in commands");
    repl::start(io::stdin().lock(), io::stdout()).unwrap();
}

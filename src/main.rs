use crate::{lexer::Lexer, parser::Parser};

mod lexer;
mod parser;
mod runtime;

fn main() {
    let script = std::fs::read_to_string("./start.leaf").unwrap();
    let lexer = Lexer::new(script.chars());
    let mut parser = Parser::new(lexer.map(Result::unwrap));
    let block = parser.parse().unwrap();
    println!("{block}");
}

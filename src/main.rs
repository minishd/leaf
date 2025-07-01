use std::time::Instant;

use crate::{lexer::Lexer, parser::Parser};

mod lexer;
mod parser;

fn main() {
    let script = std::fs::read_to_string("./start.leaf").unwrap();
    let lexer = Lexer::new(script.chars());
    let mut parser = Parser::new(lexer.map(Result::unwrap));
    let start = Instant::now();
    let block = parser.parse().unwrap();
    println!("Parse took {:?}", start.elapsed());
    parser::util::display(parser::Expr::Block(block));
}

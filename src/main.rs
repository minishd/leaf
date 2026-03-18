use std::time::Instant;

use crate::{lexer::Lexer, parser::Parser};

mod compiler;
mod kind;
mod lexer;
mod parser;
mod vm;

fn main() {
    // lexer
    let script = std::fs::read_to_string("./start.lf").unwrap();
    let lexer = Lexer::new(script.chars());
    let mut parser = Parser::new(lexer.map(Result::unwrap));

    // parser
    let start = Instant::now();
    let block = parser.parse().unwrap();
    println!("Parse took {:?}", start.elapsed());
    parser::util::display(&block);
}

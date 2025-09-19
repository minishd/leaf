use std::time::Instant;

use crate::{lexer::Lexer, parser::Parser};

mod compiler;
mod kind;
mod lexer;
mod parser;

fn main() {
    let script = std::fs::read_to_string("./start.lf").unwrap();
    let lexer = Lexer::new(script.chars());
    let mut parser = Parser::new(lexer.map(Result::unwrap));
    let start = Instant::now();
    let block = parser.parse().unwrap();
    println!("Parse took {:?}", start.elapsed());
    let e = parser::Expr::Block(block);
    parser::util::display(&e);
    let start = Instant::now();
    let mut analysis = compiler::Analyzer::new();
    analysis.analyze(e);
    println!("Analysis took {:?}", start.elapsed());
    println!("{analysis:?}");
}

use std::{rc::Rc, time::Instant};

use crate::{
    lexer::Lexer,
    parser::{Expr, Parser},
};

mod lexer;
mod parser;
mod runtime;

fn main() {
    let script = std::fs::read_to_string("./start.leaf").unwrap();
    let lexer = Lexer::new(script.chars());
    let mut parser = Parser::new(lexer.map(Result::unwrap));
    let start = Instant::now();
    let block = parser.parse().unwrap();
    println!("Parse took {:?}", start.elapsed());
    // parser::util::display(&Rc::new(Expr::Block(Rc::new(block))));
    runtime::exec(&block).unwrap();
}

use std::time::Instant;

use crate::{lexer::Lexer, parser::Parser};

mod compiler;
mod kind;
mod lexer;
mod parser;
mod vm;

fn main() {
    let script = std::fs::read_to_string("./start.lf").unwrap();
    let lexer = Lexer::new(script.chars());
    let mut parser = Parser::new(lexer.map(Result::unwrap));

    let start = Instant::now();
    let block = parser.parse().unwrap();
    println!("Parse took {:?}", start.elapsed());
    let mut e = parser::Expr::Block(block);
    parser::util::display(&e);

    let start = Instant::now();
    compiler::analysis_demo(&mut e);
    println!("Analysis took {:?}", start.elapsed());
    parser::util::display(&e);

    let start = Instant::now();
    let insts = compiler::translation_demo(e);
    println!("Translation took {:?}", start.elapsed());
    for i in &insts {
        println!("=> {i:?}");
    }

    println!("Starting VM!!!!!!!!!!!!!!!!");
    let start = Instant::now();
    let out = vm::run(&insts);
    println!("!! Got result (in {:?}): {out:?}", start.elapsed());
}

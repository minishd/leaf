use crate::parser::{Block, Expr};

pub enum Value {
    String(String),
    Integer(i64),
    Boolean(bool),
    Nil,
    Block(Block),
}

fn eval(e: Expr) -> Value {
    todo!()
}

/// Evaluates all expressions of a block.
pub fn exec(b: Block) -> Value {
    todo!()
}

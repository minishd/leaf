use std::{
    fmt::{self, Pointer},
    iter::Peekable,
};

use crate::lexer::{Associativity, Ident, LexError, Literal, Precedence, Token};

#[derive(Debug)]
pub enum Expr {
    // Data and variables
    Assignment(Box<Expr>, Box<Expr>),
    Literal(Literal),
    // Control flow
    Call(Box<Expr>, Vec<Expr>),
    Return(Box<Expr>),
    // Runtime datatypes
    Block(Block),
    // Unary operations
    Negate(Box<Expr>),
    Not(Box<Expr>),
    // Binary operations: logical
    EqualTo(Box<Expr>, Box<Expr>),
    NotEqualTo(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    // Binary operations: comparison
    LessThan(Box<Expr>, Box<Expr>),
    LessThanOrEqualTo(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    GreaterThanOrEqualTo(Box<Expr>, Box<Expr>),
    // Binary operations: arithmetic
    Add(Box<Expr>, Box<Expr>),
    Subtract(Box<Expr>, Box<Expr>),
    Multiply(Box<Expr>, Box<Expr>),
    Divide(Box<Expr>, Box<Expr>),
    Exponent(Box<Expr>, Box<Expr>),
}
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Assignment(l, r) => write!(f, "({l} = {r})\n"),
            Expr::Literal(l) => write!(f, "{l}"),
            Expr::Call(l, r) => {
                write!(f, "{l}(")?;
                for e in r {
                    write!(f, "{e}, ")?;
                }
                write!(f, ")")?;
                Ok(())
            }
            Expr::Return(l) => {
                write!(f, "return {l}")
            }
            Expr::Block(b) => {
                write!(f, "{{\n")?;
                for e in &b.exprs {
                    write!(f, "\t{e}\n")?;
                }
                write!(f, "}}")?;
                Ok(())
            }
            Expr::Negate(l) => write!(f, "(-{l})"),
            Expr::Not(l) => write!(f, "(!{l})"),
            Expr::EqualTo(l, r) => write!(f, "({l} == {r})"),
            Expr::NotEqualTo(l, r) => write!(f, "({l} != {r})"),
            Expr::And(l, r) => write!(f, "({l} && {r})"),
            Expr::Or(l, r) => write!(f, "({l} !|| {r})"),
            Expr::LessThan(l, r) => write!(f, "({l} < {r})"),
            Expr::LessThanOrEqualTo(l, r) => write!(f, "({l} <= {r})"),
            Expr::GreaterThan(l, r) => write!(f, "({l} > {r})"),
            Expr::GreaterThanOrEqualTo(l, r) => write!(f, "({l} >= {r})"),
            Expr::Add(l, r) => write!(f, "({l} + {r})"),
            Expr::Subtract(l, r) => write!(f, "({l} - {r})"),
            Expr::Multiply(l, r) => write!(f, "({l} * {r})"),
            Expr::Divide(l, r) => write!(f, "({l} / {r})"),
            Expr::Exponent(l, r) => write!(f, "({l} ^ {r})"),
        }
    }
}

#[derive(Debug, Default)]
pub struct Block {
    exprs: Vec<Expr>,
}
impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for e in &self.exprs {
            e.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token),
    UnexpectedEnd,
    LexError(LexError),
}
impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        Self::LexError(err)
    }
}

pub type Result<T> = std::result::Result<T, ParseError>;

pub struct Parser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
}
impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: I) -> Self {
        let tokens = tokens.peekable();
        Self { tokens }
    }

    fn eat(&mut self) {
        self.next_unwrap();
    }
    fn next_unwrap(&mut self) -> Token {
        self.try_next().unwrap()
    }
    fn try_peek(&mut self) -> Result<&Token> {
        self.tokens.peek().ok_or(ParseError::UnexpectedEnd)
    }
    fn try_next(&mut self) -> Result<Token> {
        self.tokens.next().ok_or(ParseError::UnexpectedEnd)
    }

    fn parse_expr(&mut self, min_prec: Precedence, in_group: bool) -> Result<Box<Expr>> {
        let mut lhs = match self.try_next()? {
            // literal
            Token::Literal(lit) => Box::new(Expr::Literal(lit)),
            // start of group
            Token::ParenOpen => {
                // begin a new expr parse (group mode)
                let e = self.parse_expr(Precedence::Min, true)?;
                // eat closing paren
                self.eat();
                e
            }
            // start of a block
            Token::CurlyOpen => {
                let b = self.parse_block(true)?;
                // skip curly brace
                self.eat();
                Box::new(Expr::Block(b))
            }
            // return
            Token::Return => Box::new(Expr::Return(self.parse_expr(Precedence::Min, false)?)),
            // not
            Token::Not => Box::new(Expr::Not(self.parse_expr(Precedence::Min, false)?)),
            // unexpected token
            t => return Err(ParseError::UnexpectedToken(t)),
        };

        loop {
            let op = match self.try_peek() {
                // end (group)
                Ok(Token::ParenClose) if in_group => break,
                // end (stream)
                Err(_) if !in_group => break,
                // operator
                Ok(t) if t.precedence().is_some() => t,
                // unexpected token (stop trying to parse)
                Ok(_) => break,
                // unexpected end
                Err(err) => return Err(err),
            };

            let (prec, assoc) = op.precedence().unwrap();

            // break if this op is meant for previous recursion
            // or it's equal and we would prefer to build leftward..
            if prec < min_prec || (prec == min_prec && assoc == Associativity::Left) {
                break;
            }

            // we're handling this op so advance the parser
            let op = self.next_unwrap();
            // parse rightward expr
            let rhs = self.parse_expr(prec, in_group)?;

            // join to lhs
            lhs = Box::new(match op {
                // equality
                Token::EqualTo => Expr::EqualTo(lhs, rhs),
                Token::NotEqualTo => Expr::NotEqualTo(lhs, rhs),
                // relational
                Token::LessThan => Expr::LessThan(lhs, rhs),
                Token::LessThanOrEqualTo => Expr::LessThan(lhs, rhs),
                Token::GreaterThan => Expr::LessThan(lhs, rhs),
                Token::GreaterThanOrEqualTo => Expr::LessThan(lhs, rhs),
                // logical
                Token::And => Expr::And(lhs, rhs),
                Token::Or => Expr::Or(lhs, rhs),
                // add, subtract
                Token::Plus => Expr::Add(lhs, rhs),
                Token::Minus => Expr::Subtract(lhs, rhs),
                // multiply, divide
                Token::Star => Expr::Multiply(lhs, rhs),
                Token::Slash => Expr::Divide(lhs, rhs),
                // exponent
                Token::Caret => Expr::Exponent(lhs, rhs),
                // assignment
                Token::Equals => Expr::Assignment(lhs, rhs),
                // unreachable as all tokens with precedences are covered above
                _ => unreachable!(),
            });
        }

        Ok(lhs)
    }
    fn parse_block(&mut self, in_block: bool) -> Result<Block> {
        let mut exprs = Vec::new();
        loop {
            match self.try_peek() {
                // end (block)
                Ok(Token::CurlyClose) if in_block => break,
                // end (stream) lpwkey idk if this is a good way to check for error
                // need to add error nodes anyway so whatever
                Err(ParseError::UnexpectedEnd) if !in_block => break,

                // try to parse expr
                Ok(_) => exprs.push(*self.parse_expr(Precedence::Min, false)?),

                // invalid
                Err(err) => return Err(err),
            }
        }
        Ok(Block { exprs })
    }
    pub fn parse(&mut self) -> Result<Block> {
        self.parse_block(false)
    }
}

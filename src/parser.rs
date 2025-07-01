use std::{fmt, iter::Peekable};

use crate::lexer::{Associativity, LexError, Literal, Precedence, Token};

pub mod util;

#[derive(Debug)]
pub enum Expr {
    // Data and variables
    Assignment(Box<Expr>, Box<Expr>),
    Literal(Literal),
    // Runtime datatypes
    Block(Block),
    // Control flow
    Return(Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
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

#[derive(Debug, Default)]
pub struct Block {
    pub exprs: Vec<Expr>,
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
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedEnd => write!(f, "unexpected end of token stream"),
            Self::UnexpectedToken(t) => write!(f, "unexpected token: {t:?}"),
            Self::LexError(err) => write!(f, "error while lexing: {err}"),
        }
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
            // unary ops!! (prefix)
            t if t.prefix_precedence().is_some() => {
                let prec = t.prefix_precedence().unwrap();
                let rhs = self.parse_expr(prec, in_group)?;
                Box::new(match t {
                    Token::Minus => Expr::Negate(rhs),
                    Token::Not => Expr::Not(rhs),
                    Token::Return => Expr::Return(rhs),
                    _ => unreachable!(),
                })
            }
            // unexpected token
            t => return Err(ParseError::UnexpectedToken(t)),
        };

        loop {
            let op = match self.try_peek() {
                // end (group)
                Ok(Token::ParenClose) if in_group => break,
                // end (stream)
                Err(_) if !in_group => break,
                // function call
                Ok(Token::ParenOpen) => {
                    // eat opening paren
                    self.eat();

                    let mut exprs = Vec::new();
                    while !matches!(self.try_peek()?, Token::ParenClose) {
                        exprs.push(*self.parse_expr(Precedence::Min, false)?);

                        // Continue if there is a comma,
                        // ignore closing parens
                        match self.try_peek()? {
                            Token::Comma => self.eat(),
                            Token::ParenClose => {}
                            _ => return Err(ParseError::UnexpectedToken(self.next_unwrap())),
                        }
                    }
                    // eat closing paren
                    self.eat();

                    lhs = Box::new(Expr::Call(lhs, exprs));
                    continue;
                }
                // operator
                Ok(t) if t.infix_precedence().is_some() => t,
                // unexpected token (stop trying to parse)
                Ok(_) => break,
                // unexpected end
                Err(err) => return Err(err),
            };

            let (prec, assoc) = op.infix_precedence().unwrap();

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
                Token::LessThanOrEqualTo => Expr::LessThanOrEqualTo(lhs, rhs),
                Token::GreaterThan => Expr::GreaterThan(lhs, rhs),
                Token::GreaterThanOrEqualTo => Expr::GreaterThanOrEqualTo(lhs, rhs),
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

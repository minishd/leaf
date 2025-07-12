use std::{fmt, iter::Peekable};

use crate::lexer::{Associativity, LexError, Literal, Precedence, Token};

pub mod util;

#[derive(Debug)]
pub enum Expr {
    // Data and variables
    Assignment(Box<Expr>, Box<Expr>),
    Literal(Literal),
    // Non-literal datatypes
    Block(Block),
    Func(Vec<Expr>, Box<Expr>),
    // Control flow
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
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
    saw_eol: bool,
}
impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: I) -> Self {
        let tokens = tokens.peekable();
        Self {
            tokens,
            saw_eol: false,
        }
    }

    fn eat(&mut self) {
        self.next_unwrap();
    }
    fn next_unwrap(&mut self) -> Token {
        self.try_next().unwrap()
    }
    fn skip_eol(&mut self) -> bool {
        let mut did_skip = false;

        while matches!(self.tokens.peek(), Some(Token::Eol)) {
            self.tokens.next();
            did_skip = true;
        }

        return did_skip;
    }
    fn try_peek(&mut self) -> Result<&Token> {
        // Peek doesn't advance the token stream, so
        // don't allow it to unset the EOL flag
        if self.skip_eol() {
            self.saw_eol = true;
        }
        self.tokens.peek().ok_or(ParseError::UnexpectedEnd)
    }
    fn try_next(&mut self) -> Result<Token> {
        self.saw_eol = self.skip_eol();
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
                // parse function
                if matches!(t, Token::Func) {
                    // expect opening paren
                    let next = self.try_next()?;
                    if !matches!(next, Token::ParenOpen) {
                        return Err(ParseError::UnexpectedToken(next));
                    }
                    // parse args
                    let args = self.parse_args()?;
                    // expect closing paren
                    if !matches!(self.try_peek(), Ok(Token::ParenClose)) {
                        return Err(ParseError::UnexpectedToken(self.next_unwrap()));
                    }
                    self.eat();
                    // parse body
                    let body = self.parse_expr(prec, in_group)?;
                    // pack
                    Box::new(Expr::Func(args, body))
                } else {
                    let rhs = self.parse_expr(prec, in_group)?;
                    Box::new(match t {
                        Token::Minus => Expr::Negate(rhs),
                        Token::Not => Expr::Not(rhs),
                        Token::Return => Expr::Return(rhs),
                        Token::If => {
                            // parse the true case
                            let true_case = self.parse_expr(prec, in_group)?;
                            // and maybe a false case
                            let false_case = matches!(self.try_peek(), Ok(Token::Else))
                                .then(|| {
                                    self.eat();
                                    self.parse_expr(prec, in_group)
                                })
                                .transpose()?;
                            // pack
                            Expr::If(rhs, true_case, false_case)
                        }
                        _ => unreachable!(),
                    })
                }
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
                // unexpected end
                Err(err) => return Err(err),

                // operator
                Ok(t) if t.infix_precedence().is_some() => t,

                // function call
                Ok(Token::ParenOpen) => {
                    if self.saw_eol {
                        break;
                    }

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

                // unexpected token (stop trying to parse)
                Ok(_) => break,
            };

            let (prec, assoc) = op.infix_precedence().unwrap();

            // break if this op is meant for previous recursion
            // or it's equal and we prefer to build leftward
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
    fn parse_args(&mut self) -> Result<Vec<Expr>> {
        let mut exprs = Vec::new();
        while !matches!(self.try_peek(), Ok(Token::ParenClose)) {
            // try to parse expr
            exprs.push(*self.parse_expr(Precedence::Min, false)?);
            // advance
            let next = self.try_next()?;
            // check if its the end
            if matches!(next, Token::ParenClose) {
                break;
            }
            // expect comma
            if !matches!(next, Token::Comma) {
                return Err(ParseError::UnexpectedToken(next));
            }
        }
        Ok(exprs)
    }
    fn parse_block(&mut self, in_block: bool) -> Result<Block> {
        let mut exprs = Vec::new();
        loop {
            match self.try_peek() {
                // end (block)
                Ok(Token::CurlyClose) if in_block => break,
                // end (stream)
                Err(ParseError::UnexpectedEnd) if !in_block => break,

                // try to parse expr
                Ok(_) => {
                    exprs.push(*self.parse_expr(Precedence::Min, false)?);
                    // expect eol or eof
                    if !matches!(self.try_peek(), Err(ParseError::UnexpectedEnd)) && !self.saw_eol {
                        return Err(ParseError::UnexpectedToken(self.next_unwrap()));
                    }
                }

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

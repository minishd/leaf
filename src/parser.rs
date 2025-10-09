use std::{fmt, iter::Peekable};

use crate::{
    compiler::FuncStat,
    kind::Kind,
    lexer::{Associativity, LexError, Literal, Precedence, Token, TokenKind},
};

pub mod util;

#[derive(Debug, Clone)]
pub enum Expr {
    // Data and variables
    Assign(Box<Expr>, Box<Expr>),
    Literal(Literal),
    // Non-literal datatypes
    Block(Block),
    Func(Vec<Expr>, Box<Expr>, Option<FuncStat>),
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
    Modulo(Box<Expr>, Box<Expr>),
    // Binary operations: arithmetic w/ assignment
    AddAssign(Box<Expr>, Box<Expr>),
    SubtractAssign(Box<Expr>, Box<Expr>),
    MultiplyAssign(Box<Expr>, Box<Expr>),
    DivideAssign(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Default, Clone)]
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

    fn expect_next(&mut self, kind: TokenKind) -> Result<()> {
        let t = self.try_next()?;

        if t.kind() != kind {
            return Err(ParseError::UnexpectedToken(t));
        }

        Ok(())
    }
    fn is_next(&mut self, kind: Option<TokenKind>) -> bool {
        match self.try_peek() {
            Ok(t) if Some(t.kind()) == kind => true,
            Ok(_) => false,

            Err(ParseError::UnexpectedEnd) if kind.is_none() => true,
            Err(_) => unreachable!(),
        }
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
                let exprs = self.parse_until(Some(TokenKind::CurlyClose))?;
                // skip curly brace
                self.eat();
                Box::new(Expr::Block(Block { exprs }))
            }

            // unary ops!! (prefix)
            t if t.prefix_precedence().is_some() => {
                let prec = t.prefix_precedence().unwrap();
                match t {
                    // parse function
                    Token::Func => {
                        // parse args
                        self.expect_next(TokenKind::ParenOpen)?;
                        let args = self
                            .parse_delimited_until(TokenKind::Comma, Some(TokenKind::ParenClose))?;
                        self.eat();
                        // parse body
                        let body = self.parse_expr(prec, in_group)?;
                        // pack
                        Box::new(Expr::Func(args, body, None))
                    }
                    // parse if
                    Token::If => {
                        // parse the condition
                        let cond = self.parse_expr(Precedence::Min, false)?;
                        // parse the true case
                        let true_case = self.parse_expr(Precedence::Min, in_group)?;
                        // and maybe a false case
                        let false_case = matches!(self.try_peek(), Ok(Token::Else))
                            .then(|| {
                                self.eat();
                                self.parse_expr(Precedence::Min, in_group)
                            })
                            .transpose()?;
                        // pack
                        Box::new(Expr::If(cond, true_case, false_case))
                    }

                    // another op
                    _ => {
                        let rhs = self.parse_expr(prec, in_group)?;
                        Box::new(match t {
                            Token::Minus => Expr::Negate(rhs),
                            Token::Not => Expr::Not(rhs),
                            Token::Return => Expr::Return(rhs),

                            _ => unreachable!(),
                        })
                    }
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
                    // eat opening paren
                    self.eat();

                    let exprs =
                        self.parse_delimited_until(TokenKind::Comma, Some(TokenKind::ParenClose))?;

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
                Token::PlusEquals => Expr::AddAssign(lhs, rhs),
                Token::MinusEquals => Expr::SubtractAssign(lhs, rhs),
                // multiply, divide
                Token::Star => Expr::Multiply(lhs, rhs),
                Token::Slash => Expr::Divide(lhs, rhs),
                Token::StarEquals => Expr::MultiplyAssign(lhs, rhs),
                Token::SlashEquals => Expr::DivideAssign(lhs, rhs),
                // exponent, modulo
                Token::StarStar => Expr::Exponent(lhs, rhs),
                Token::Percent => Expr::Modulo(lhs, rhs),
                // assignment
                Token::Equals => Expr::Assign(lhs, rhs),
                // unreachable as all tokens with precedences are covered above
                _ => unreachable!(),
            });
        }

        Ok(lhs)
    }

    fn parse_until(&mut self, until: Option<TokenKind>) -> Result<Vec<Expr>> {
        let mut exprs = Vec::new();

        while !self.is_next(until) {
            // try to parse expr
            exprs.push(*self.parse_expr(Precedence::Min, false)?);

            // check for end
            if self.is_next(until) {
                break;
            }
        }

        Ok(exprs)
    }

    fn parse_delimited_until(
        &mut self,
        delim: TokenKind,
        until: Option<TokenKind>,
    ) -> Result<Vec<Expr>> {
        let mut exprs = Vec::new();

        while !self.is_next(until) {
            // skip delimiter
            if self.is_next(Some(delim)) {
                self.eat();
                continue;
            }

            // try to parse expr
            exprs.push(*self.parse_expr(Precedence::Min, false)?);

            // check for end
            if self.is_next(until) {
                break;
            }

            // check for delim
            self.expect_next(delim)?;
        }
        Ok(exprs)
    }

    pub fn parse(&mut self) -> Result<Block> {
        let exprs = self.parse_until(None)?;
        Ok(Block { exprs })
    }
}

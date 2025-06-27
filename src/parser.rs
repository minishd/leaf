use std::iter::Peekable;

use crate::lexer::{Ident, LexError, Literal, Token};

#[derive(Debug)]
pub enum Expr {
    // Data and variables
    Assignment(Ident, Box<Expr>),
    Literal(Literal),
    // Control flow
    Call(Ident, Vec<Expr>),
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
}

#[derive(Debug, Default)]
pub struct Block {
    exprs: Vec<Expr>,
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
        self.tokens
            .next()
            .inspect(|t| println!("next= {t:?}"))
            .ok_or(ParseError::UnexpectedEnd)
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        Ok(match self.try_next()? {
            // Assignment
            Token::Literal(Literal::Ident(id)) if matches!(self.try_peek(), Ok(Token::Equals)) => {
                self.eat();
                let rhs = self.parse_expr()?;
                Expr::Assignment(id, Box::new(rhs))
            }

            // Block call
            Token::Literal(Literal::Ident(id))
                if matches!(self.try_peek(), Ok(Token::ParenOpen)) =>
            {
                self.eat();
                let mut args = Vec::new();
                while !matches!(self.try_peek()?, Token::ParenClose) {
                    args.push(self.parse_expr()?);

                    // require comma for next arg if it's not the end
                    let tk = self.try_peek()?;
                    if matches!(tk, Token::Comma) {
                        self.eat();
                    } else if !matches!(tk, Token::ParenClose) {
                        // no comma OR closing paren.. bad...
                        return Err(ParseError::UnexpectedToken(self.next_unwrap()));
                    }
                }
                // Eat closing paren
                self.eat();
                Expr::Call(id, args)
            }

            Token::Literal(lit) => match self.try_peek() {
                // Binary Op: equal to (lit, expr)
                Ok(Token::EqualTo) => {
                    self.eat();
                    Expr::EqualTo(Box::new(Expr::Literal(lit)), Box::new(self.parse_expr()?))
                }
                // Binary Op: not equal to (lit, expr)
                Ok(Token::NotEqualTo) => {
                    self.eat();
                    Expr::NotEqualTo(Box::new(Expr::Literal(lit)), Box::new(self.parse_expr()?))
                }

                // Binary Op: less than (lit, expr)
                Ok(Token::LessThan) => {
                    self.eat();
                    Expr::LessThan(Box::new(Expr::Literal(lit)), Box::new(self.parse_expr()?))
                }
                // Binary Op: less than or equal to (lit, expr)
                Ok(Token::LessThanOrEqualTo) => {
                    self.eat();
                    Expr::LessThanOrEqualTo(
                        Box::new(Expr::Literal(lit)),
                        Box::new(self.parse_expr()?),
                    )
                }
                // Binary Op: greater than (lit, expr)
                Ok(Token::GreaterThan) => {
                    self.eat();
                    Expr::GreaterThan(Box::new(Expr::Literal(lit)), Box::new(self.parse_expr()?))
                }
                // Binary Op: greater than or equal to (lit, expr)
                Ok(Token::GreaterThanOrEqualTo) => {
                    self.eat();
                    Expr::GreaterThanOrEqualTo(
                        Box::new(Expr::Literal(lit)),
                        Box::new(self.parse_expr()?),
                    )
                }

                // Binary Op: and (lit, expr)
                Ok(Token::And) => {
                    self.eat();
                    Expr::And(Box::new(Expr::Literal(lit)), Box::new(self.parse_expr()?))
                }
                // Binary Op: or (lit, expr)
                Ok(Token::Or) => {
                    self.eat();
                    Expr::Or(Box::new(Expr::Literal(lit)), Box::new(self.parse_expr()?))
                }

                // Literal
                _ => Expr::Literal(lit),
            },

            // Unary Op: negate
            Token::Minus => Expr::Negate(Box::new(self.parse_expr()?)),
            // Unary Op: not
            Token::Not => Expr::Not(Box::new(self.parse_expr()?)),

            // Start of a block
            Token::CurlyOpen => {
                let mut exprs = Vec::new();
                while !matches!(self.try_peek()?, Token::CurlyClose) {
                    exprs.push(self.parse_expr()?);
                }
                self.eat();
                Expr::Block(Block { exprs })
            }

            // Return
            Token::Return => Expr::Return(Box::new(self.parse_expr()?)),

            t => return Err(ParseError::UnexpectedToken(t)),
        })
    }
    pub fn parse_root(&mut self) -> Result<Block> {
        let mut exprs = Vec::new();
        while self.try_peek().is_ok() {
            exprs.push(self.parse_expr()?);
        }
        Ok(Block { exprs })
    }
}

use std::{
    fmt,
    iter::Peekable,
    num::{ParseFloatError, ParseIntError},
};

use crate::{compiler::RefStat, kinds};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ident(String);
impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Nil,
    Ident(Ident, Option<RefStat>),
}
impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::String(s) => write!(f, "\"{s}\""),
            Literal::Integer(n) => write!(f, "{n}"),
            Literal::Float(n) => write!(f, "{n}"),
            Literal::Boolean(b) => write!(f, "{b}"),
            Literal::Ident(id, ref_stat) => write!(
                f,
                "{id}{}",
                ref_stat
                    .as_ref()
                    .map(|rs| format!(
                        "@{}{}/{}",
                        rs.stat.is_shared.get().then_some("sh+").unwrap_or(""),
                        rs.now,
                        rs.stat.total.get()
                    ))
                    .unwrap_or_default()
            ),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

kinds!(
    Token,
    TokenKind,
    Equals,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    StarStar,
    PlusEquals,
    MinusEquals,
    StarEquals,
    SlashEquals,
    CurlyOpen,
    CurlyClose,
    ParenOpen,
    ParenClose,
    Comma,
    Eol,
    Func,
    If,
    Else,
    Return,
    Not,
    EqualTo,
    NotEqualTo,
    And,
    Or,
    LessThan,
    LessThanOrEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
    Literal(Literal = Literal::Nil),
);
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Precedence {
    Min,
    Assign,
    WithAssign,
    Logical,
    Equality,
    Relational,
    AddSub,
    MulDivMod,
    Pow,
    Prefix,
}
#[derive(PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
}
impl Token {
    pub fn prefix_precedence(&self) -> Option<Precedence> {
        Some(match self {
            Token::Return | Token::If | Token::Func | Token::Minus | Token::Not => {
                Precedence::Prefix
            }
            _ => return None,
        })
    }
    pub fn infix_precedence(&self) -> Option<(Precedence, Associativity)> {
        Some(match self {
            Token::EqualTo | Token::NotEqualTo => (Precedence::Equality, Associativity::Left),
            Token::LessThan
            | Token::LessThanOrEqualTo
            | Token::GreaterThan
            | Token::GreaterThanOrEqualTo => (Precedence::Relational, Associativity::Left),
            Token::And | Token::Or => (Precedence::Logical, Associativity::Left),
            Token::Plus | Token::Minus => (Precedence::AddSub, Associativity::Left),
            Token::Star | Token::Slash | Token::Percent => {
                (Precedence::MulDivMod, Associativity::Left)
            }
            Token::StarStar => (Precedence::Pow, Associativity::Right),
            Token::Equals => (Precedence::Assign, Associativity::Right),
            Token::PlusEquals | Token::MinusEquals | Token::StarEquals | Token::SlashEquals => {
                (Precedence::WithAssign, Associativity::Right)
            }
            _ => return None,
        })
    }
}

#[derive(Debug)]
pub enum LexError {
    InvalidInteger(ParseIntError),
    InvalidFloat(ParseFloatError),
    InvalidEscape(char),
    UnexpectedCharacter(char),
    UnexpectedEnd,
}
impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidInteger(err) => write!(f, "invalid integer: {err}"),
            Self::InvalidFloat(err) => write!(f, "invalid float: {err}"),
            Self::UnexpectedEnd => write!(f, "unexpected end of source"),
            Self::UnexpectedCharacter(c) => write!(f, "unexpected char '{c}'"),
            Self::InvalidEscape(c) => write!(f, "\"\\{c}\" is not a valid string escape"),
        }
    }
}
impl From<ParseIntError> for LexError {
    fn from(err: ParseIntError) -> Self {
        Self::InvalidInteger(err)
    }
}
impl From<ParseFloatError> for LexError {
    fn from(err: ParseFloatError) -> Self {
        Self::InvalidFloat(err)
    }
}

pub type Result<T> = std::result::Result<T, LexError>;

pub struct Lexer<I>
where
    I: Iterator<Item = char>,
{
    chars: Peekable<I>,
}

#[allow(clippy::unnecessary_wraps)]
fn t(tk: Token) -> Option<Result<Token>> {
    Some(Ok(tk))
}

impl<I> Lexer<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(chars: I) -> Self {
        let chars = chars.peekable();
        Self { chars }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    fn next(&mut self) -> Option<char> {
        self.chars.next()
    }
    fn next_unwrap(&mut self) -> char {
        match self.next() {
            Some(c) => c,
            None => unreachable!("called next_unwrap with nothing ahead"),
        }
    }

    fn try_peek(&mut self) -> Result<char> {
        self.peek().ok_or(LexError::UnexpectedEnd)
    }
    fn try_eat_peek(&mut self) -> Result<char> {
        self.eat_peek().ok_or(LexError::UnexpectedEnd)
    }

    fn eat(&mut self) {
        self.next();
    }
    #[allow(clippy::unnecessary_wraps)]
    fn eat_to(&mut self, tk: Token) -> Option<Result<Token>> {
        self.eat();
        Some(Ok(tk))
    }
    fn eat_peek(&mut self) -> Option<char> {
        self.eat();
        self.peek()
    }

    fn lex_whitespace(&mut self) -> Option<char> {
        loop {
            match self.peek()? {
                ' ' | '\t' | '\n' | '\r' => self.eat(),
                _ => break self.peek(),
            }
        }
    }

    fn lex_word(&mut self) -> Token {
        let mut word = String::new();

        while let Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_') = self.peek() {
            word.push(self.next_unwrap());
        }

        match word.as_str() {
            "func" => Token::Func,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            "true" => Token::Literal(Literal::Boolean(true)),
            "false" => Token::Literal(Literal::Boolean(false)),
            "nil" => Token::Literal(Literal::Nil),
            _ => Token::Literal(Literal::Ident(Ident(word), Option::default())),
        }
    }

    fn lex_number(&mut self) -> Result<Token> {
        let mut n_str = String::new();

        // we don't lex negatives. the impl for that is
        // a negation of a positive number at runtime.
        // maybe that's kind of stupid though, lol
        let mut is_float = false;
        while let Some('0'..='9' | '.') = self.peek() {
            if self.peek() == Some('.') {
                is_float = true;
            }
            n_str.push(self.next_unwrap());
        }

        let lit = if is_float {
            Literal::Float(n_str.parse()?)
        } else {
            Literal::Integer(n_str.parse()?)
        };

        Ok(Token::Literal(lit))
    }

    fn lex_string(&mut self) -> Result<Token> {
        let delim = self.next_unwrap();

        let mut str = String::new();

        loop {
            match self.try_peek()? {
                '\\' => match self.try_eat_peek()? {
                    'n' => {
                        self.eat();
                        str.push('\n');
                    }
                    c => {
                        break Err(LexError::InvalidEscape(c));
                    }
                },
                c if c == delim => {
                    self.eat();
                    break Ok(Token::Literal(Literal::String(str)));
                }
                _ => str.push(self.next_unwrap()),
            }
        }
    }

    fn lex(&mut self) -> Option<Result<Token>> {
        loop {
            break match self.lex_whitespace()? {
                // { and } start/end of code block
                '{' => self.eat_to(Token::CurlyOpen),
                '}' => self.eat_to(Token::CurlyClose),

                // ( and ) start/end of groups
                '(' => self.eat_to(Token::ParenOpen),
                ')' => self.eat_to(Token::ParenClose),

                // + add
                // or += add eq
                '+' => match self.eat_peek() {
                    Some('=') => self.eat_to(Token::PlusEquals),
                    _ => t(Token::Plus),
                },

                // - subtract
                // or -= sub eq
                '-' => match self.eat_peek() {
                    Some('=') => self.eat_to(Token::MinusEquals),
                    _ => t(Token::Minus),
                },

                // * multiply
                // or *= mult eq
                // or ** pow
                '*' => match self.eat_peek() {
                    Some('=') => self.eat_to(Token::StarEquals),
                    Some('*') => self.eat_to(Token::StarStar),
                    _ => t(Token::Star),
                },

                // / divide
                // or /= div eq
                // or // comment
                '/' => match self.eat_peek() {
                    Some('=') => self.eat_to(Token::SlashEquals),
                    Some('/') => {
                        // skip the rest of the line
                        // this leaves the newline btw
                        while !matches!(self.peek(), Some('\n') | None) {
                            self.eat();
                        }
                        continue;
                    }
                    _ => t(Token::Slash),
                },

                // % modulo
                '%' => self.eat_to(Token::Percent),

                // , comma
                ',' => self.eat_to(Token::Comma),

                // = equals
                // or == equal to
                '=' => match self.eat_peek() {
                    Some('=') => self.eat_to(Token::EqualTo),
                    _ => t(Token::Equals),
                },

                // ! not
                // or != not equal to
                '!' => match self.eat_peek() {
                    Some('=') => self.eat_to(Token::NotEqualTo),
                    _ => t(Token::Not),
                },

                // && and
                '&' if matches!(self.eat_peek(), Some('&')) => self.eat_to(Token::And),

                // || or
                '|' if matches!(self.eat_peek(), Some('|')) => self.eat_to(Token::Or),

                // > greater than
                // or >= greater than/equal to
                '>' => match self.eat_peek() {
                    Some('=') => self.eat_to(Token::GreaterThanOrEqualTo),
                    _ => t(Token::GreaterThan),
                },

                // < less than
                // or <= less than/equal to
                '<' => match self.eat_peek() {
                    Some('=') => self.eat_to(Token::LessThanOrEqualTo),
                    _ => t(Token::LessThan),
                },

                // a-zA-Z_ start of word
                'a'..='z' | 'A'..='Z' | '_' => Some(Ok(self.lex_word())),

                // 0-9 integer
                '0'..='9' | '.' => Some(self.lex_number()),

                // " strings
                '"' => Some(self.lex_string()),

                // unexpected character
                c => Some(Err(LexError::UnexpectedCharacter(c))),
            };
        }
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = char>,
{
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex()
    }
}

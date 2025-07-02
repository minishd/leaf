use std::{cell::LazyCell, collections::HashMap, fmt, fmt::Write as _, rc::Rc};

use crate::{
    lexer::{Ident, Literal},
    parser::{Block, Expr},
};

macro_rules! type_error {
    () => {
        return Err(RuntimeError::Type.into())
    };
}
macro_rules! uncallable {
    () => {
        return Err(RuntimeError::Uncallable.into())
    };
}

use Expr as E;
use Literal as L;
use Value as V;

fn builtin_print(args: Args) -> RuntimeResult {
    let mut result = String::new();
    for v in args {
        let _ = write!(result, "{v}\t");
    }
    println!("{result}");
    Ok(V::nil())
}
fn builtin_if(args: Args) -> RuntimeResult {
    let Some(Value::Boolean(cond)) = args.get(0).map(|v| &**v) else {
        type_error!()
    };
    let cond = *cond;

    let b_else = args.get(2).map(|v| &**v);
    let b_if = args.get(1).map(|v| &**v);

    let empty = Vec::new();
    Ok(match (b_if, b_else) {
        (Some(V::Block(b_if)), _) if cond => exec_block(&b_if, empty)?,
        (_, Some(V::Block(b_else))) if !cond => exec_block(&b_else, empty)?,
        (Some(V::Nil) | None, _) => V::nil(),
        (_, Some(V::Nil) | None) => V::nil(),
        (_, _) => uncallable!(),
    })
}

thread_local! {
    static NIL: LazyCell<Rc<Value>> = const { LazyCell::new(|| Rc::new(V::Nil)) };
    static TRUE: LazyCell<Rc<Value>> = const { LazyCell::new(|| Rc::new(V::Boolean(true))) };
    static FALSE: LazyCell<Rc<Value>> = const { LazyCell::new(|| Rc::new(V::Boolean(false))) };
}

pub enum Value {
    String(Rc<String>),
    Integer(i64),
    Boolean(bool),
    Block(Rc<Block>),
    BuiltinFn(fn(Args) -> RuntimeResult),
    Nil,
}
impl Value {
    fn bool(b: bool) -> Rc<Self> {
        if b {
            Self::bool_true()
        } else {
            Self::bool_false()
        }
    }
    fn int(i: i64) -> Rc<Self> {
        Rc::new(V::Integer(i))
    }
    fn str(s: Rc<String>) -> Rc<Self> {
        Rc::new(V::String(s))
    }
    fn block(b: Rc<Block>) -> Rc<Self> {
        Rc::new(V::Block(b))
    }
    fn builtin_fn(f: fn(Args) -> RuntimeResult) -> Rc<Self> {
        Rc::new(V::BuiltinFn(f))
    }
    fn bool_true() -> Rc<Self> {
        TRUE.with(|v| (**v).clone())
    }
    fn bool_false() -> Rc<Self> {
        FALSE.with(|v| (**v).clone())
    }
    fn nil() -> Rc<Self> {
        NIL.with(|v| (**v).clone())
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (V::String(s1), V::String(s2)) => s1 == s2,
            (V::Integer(i1), V::Integer(i2)) => i1 == i2,
            (V::Boolean(b1), V::Boolean(b2)) => b1 == b2,
            (V::Nil, V::Nil) => true,
            _ => false,
        }
    }
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            V::String(s) => write!(f, "{s}"),
            V::Integer(i) => write!(f, "{i}"),
            V::Boolean(b) => write!(f, "{b}"),
            V::Block(_b) => write!(f, "<block>"),
            V::BuiltinFn(_f) => write!(f, "<builtin>"),
            V::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    Type,
    Uncallable,
}
impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Type => write!(f, "type error"),
            Self::Uncallable => write!(f, "uncallable"),
        }
    }
}
pub type RuntimeResult = Result<Rc<Value>, RuntimeError>;

type Args = Vec<Rc<Value>>;

enum EvalOutcome {
    EarlyReturn(Rc<Value>),
    Error(RuntimeError),
}
impl From<RuntimeError> for EvalOutcome {
    fn from(err: RuntimeError) -> Self {
        Self::Error(err)
    }
}

fn eval(scope: &mut HashMap<Ident, Rc<Value>>, e: &Rc<Expr>) -> Result<Rc<Value>, EvalOutcome> {
    Ok(match &**e {
        E::Assignment(l, r) => {
            let Expr::Literal(Literal::Ident(id)) = &**l else {
                type_error!()
            };
            let v = eval(scope, r)?;
            scope.insert(id.clone(), v.clone());
            v
        }
        E::Literal(L::Ident(id)) => match id.as_ref() {
            "print" => V::builtin_fn(builtin_print),
            "if" => V::builtin_fn(builtin_if),
            _ => scope.get(id).cloned().unwrap_or_else(V::nil),
        },
        E::Literal(L::String(s)) => V::str(s.clone()),
        E::Literal(L::Integer(i)) => V::int(*i),
        E::Literal(L::Boolean(b)) => V::bool(*b),
        E::Literal(L::Nil) => V::nil(),
        E::Block(b) => V::block(b.clone()),
        E::Return(r) => {
            let r = eval(scope, r)?;
            return Err(EvalOutcome::EarlyReturn(r));
        }
        E::Call(l, r) => {
            let l = eval(scope, l)?;
            let args = r.iter().map(|e| eval(scope, e)).collect::<Result<_, _>>()?;
            match &*l {
                V::BuiltinFn(f) => f(args)?,
                V::Block(b) => exec_block(b, args)?,
                _ => uncallable!(),
            }
        }
        E::Negate(r) => {
            let l = eval(scope, r)?;
            let V::Integer(i) = *l else { type_error!() };
            V::int(-i)
        }
        E::Not(r) => {
            let l = eval(scope, r)?;
            let V::Boolean(b) = *l else { type_error!() };
            V::bool(!b)
        }
        E::EqualTo(l, r) => {
            let (l, r) = (eval(scope, l)?, eval(scope, r)?);
            V::bool(*l == *r)
        }
        E::NotEqualTo(l, r) => {
            let (l, r) = (eval(scope, l)?, eval(scope, r)?);
            V::bool(*l != *r)
        }
        E::And(l, r) => {
            let (l, r) = (eval(scope, l)?, eval(scope, r)?);
            V::bool(if let (V::Boolean(b1), V::Boolean(b2)) = (&*l, &*r) {
                *b1 && *b2
            } else {
                type_error!()
            })
        }
        E::Or(l, r) => {
            let (l, r) = (eval(scope, l)?, eval(scope, r)?);
            V::bool(if let (V::Boolean(b1), V::Boolean(b2)) = (&*l, &*r) {
                *b1 || *b2
            } else {
                type_error!()
            })
        }
        E::LessThan(l, r) => {
            let (l, r) = (eval(scope, l)?, eval(scope, r)?);
            V::bool(if let (V::Integer(i1), V::Integer(i2)) = (&*l, &*r) {
                i1 < i2
            } else {
                type_error!()
            })
        }
        E::LessThanOrEqualTo(l, r) => {
            let (l, r) = (eval(scope, l)?, eval(scope, r)?);
            V::bool(if let (V::Integer(i1), V::Integer(i2)) = (&*l, &*r) {
                i1 <= i2
            } else {
                type_error!()
            })
        }
        E::GreaterThan(l, r) => {
            let (l, r) = (eval(scope, l)?, eval(scope, r)?);
            V::bool(if let (V::Integer(i1), V::Integer(i2)) = (&*l, &*r) {
                i1 > i2
            } else {
                type_error!()
            })
        }
        E::GreaterThanOrEqualTo(l, r) => {
            let (l, r) = (eval(scope, l)?, eval(scope, r)?);
            V::bool(if let (V::Integer(i1), V::Integer(i2)) = (&*l, &*r) {
                i1 >= i2
            } else {
                type_error!()
            })
        }
        E::Add(l, r) => {
            let (l, r) = (eval(scope, l)?, eval(scope, r)?);
            V::int(if let (V::Integer(i1), V::Integer(i2)) = (&*l, &*r) {
                i1 + i2
            } else {
                type_error!()
            })
        }
        E::Subtract(l, r) => {
            let (l, r) = (eval(scope, l)?, eval(scope, r)?);
            V::int(if let (V::Integer(i1), V::Integer(i2)) = (&*l, &*r) {
                i1 + i2
            } else {
                type_error!()
            })
        }
        E::Multiply(l, r) => {
            let (l, r) = (eval(scope, l)?, eval(scope, r)?);
            V::int(if let (V::Integer(i1), V::Integer(i2)) = (&*l, &*r) {
                i1 * i2
            } else {
                type_error!()
            })
        }
        E::Divide(l, r) => {
            let (l, r) = (eval(scope, l)?, eval(scope, r)?);
            V::int(if let (V::Integer(i1), V::Integer(i2)) = (&*l, &*r) {
                i1 / i2
            } else {
                type_error!()
            })
        }
        E::Exponent(l, r) => {
            let (l, r) = (eval(scope, l)?, eval(scope, r)?);
            V::int(if let (V::Integer(i1), V::Integer(i2)) = (&*l, &*r) {
                i1.pow(*i2 as u32)
            } else {
                type_error!()
            })
        }
    })
}

fn exec_block(b: &Block, _args: Args) -> RuntimeResult {
    let mut scope = HashMap::new();

    for e in &b.exprs {
        match eval(&mut scope, e) {
            Ok(_) => {}
            Err(EvalOutcome::EarlyReturn(v)) => return Ok(v),
            Err(EvalOutcome::Error(err)) => return Err(err),
        }
    }

    Ok(V::nil())
}

pub fn exec(b: &Block) -> RuntimeResult {
    exec_block(b, Vec::new())
}

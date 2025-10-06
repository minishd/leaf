use std::{cell::Cell, rc::Rc};

use crate::{
    lexer::{Ident, Literal},
    parser::Expr,
};

use stack::Stack;

mod stack;

struct Scope<'a> {
    values: Vec<(Ident, Rc<RefMeta>)>,
    parent: Option<&'a Scope<'a>>,
}
impl<'a> Stack<'a> for Scope<'a> {
    type Value = (Ident, Rc<RefMeta>);
    type Input = Ident;
    type Output = Rc<RefMeta>;

    fn with_parent(parent: Option<&'a Self>) -> Self {
        Self {
            values: Vec::new(),
            parent,
        }
    }
    fn parent(&self) -> Option<&'a Self> {
        self.parent
    }
    fn values(&self) -> &Vec<Self::Value> {
        &self.values
    }
    fn values_mut(&mut self) -> &mut Vec<Self::Value> {
        &mut self.values
    }

    fn find_map(_: usize, value: &Self::Value, input: &Self::Input) -> Option<Self::Output> {
        (value.0 == *input).then_some(value.1.clone())
    }
}
impl Scope<'_> {
    fn assigned(&mut self, id: Ident) {
        if self.find(&id).is_none() {
            self.push((id, Rc::default()));
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct FuncMeta {
    pub is_unreturnable: Cell<bool>,
}
pub type FuncStat = Rc<FuncMeta>;

#[derive(Debug, Clone)]
pub struct RefStat {
    pub now: u16,
    pub stat: Rc<RefMeta>,
}

#[derive(Debug, Default)]
pub struct RefMeta {
    pub total: Cell<u16>,
    pub is_shared: Cell<bool>,
}

fn analyze(fs: &FuncStat, scope: &mut Scope, e: &mut Expr) {
    match e {
        Expr::Assign(a, b) => {
            let Expr::Literal(Literal::Ident(id, _)) = &**a else {
                panic!("invalid assignment");
            };

            // add to scope
            scope.assigned(id.clone());

            // analyse the value
            analyze(fs, scope, b);
        }
        Expr::Literal(Literal::Ident(id, ref_stat)) => {
            // lookup literal
            let Some((rs, up_levels)) = scope.find(id) else {
                panic!("unfound variable")
            };
            // increment # of uses
            rs.total.update(|c| c + 1);
            // set ref meta
            *ref_stat = Some(RefStat {
                now: rs.total.get(),
                stat: rs.clone(),
            });
            // if we used something external to this scope, note it
            if up_levels != 0 {
                fs.is_unreturnable.set(true);
                rs.is_shared.set(true);
            }
        }
        // ignore
        Expr::Literal(_) => {}
        // for recursion..
        Expr::Block(a) => {
            // blocks have their own scope
            let mut scope = Scope::with_parent(Some(scope));
            // analyze the contents in the new scope
            for e in &mut a.exprs {
                analyze(fs, &mut scope, e);
            }
        }
        Expr::Func(a, b, func_stat) => {
            // new function new context
            let fs = FuncStat::default();
            *func_stat = Some(fs.clone());
            // functions have their own scope, because they have args
            let mut scope = Scope::with_parent(Some(scope));

            // init args
            for e in a {
                let Expr::Literal(Literal::Ident(id, _)) = e else {
                    panic!("invalid arg def");
                };
                scope.assigned(id.clone());
            }

            // now analyze the body in the new scope
            analyze(&fs, &mut scope, b);
        }
        Expr::If(a, b, c) => {
            analyze(fs, scope, a);
            analyze(fs, scope, b);
            if let Some(c) = c {
                analyze(fs, scope, c);
            }
        }
        Expr::Call(a, b) => {
            analyze(fs, scope, a);
            for e in b {
                analyze(fs, scope, e);
            }
        }
        Expr::Return(a) | Expr::Negate(a) | Expr::Not(a) => analyze(fs, scope, a),
        Expr::EqualTo(a, b)
        | Expr::NotEqualTo(a, b)
        | Expr::And(a, b)
        | Expr::Or(a, b)
        | Expr::LessThan(a, b)
        | Expr::LessThanOrEqualTo(a, b)
        | Expr::GreaterThan(a, b)
        | Expr::GreaterThanOrEqualTo(a, b)
        | Expr::Add(a, b)
        | Expr::Subtract(a, b)
        | Expr::Multiply(a, b)
        | Expr::Divide(a, b)
        | Expr::Exponent(a, b)
        | Expr::Modulo(a, b)
        | Expr::AddAssign(a, b) // maybe handle these differently?
        | Expr::SubtractAssign(a, b) // when error handling is added at least
        | Expr::MultiplyAssign(a, b)
        | Expr::DivideAssign(a, b) =>{
            analyze(fs, scope, a);
            analyze(fs, scope, b);
        }
    }
}

// --- translate pass --- //

/* 1b is up? */
enum Stkval {
    /* 4b blank ; 8b offset */
    Local(u8),
    /* 4b up levels ; 8b offset */
    Shared(u8, u8),
}
/* 3b type tag */
enum Val {
    Stack(Stkval),
    /* u16 len, LEN data */
    String(String),
    /* 1b returnability, 4b arity, insts */
    Func(bool, Vec<Inst>),
    /* 1b value */
    Bool(bool),
    /* i64 data */
    Int64(i64),
    /* f64 data */
    Float64(f64),
    /* ... */
    Nil,
}
/* 5b inst type */
enum Inst {
    /* ... */
    Copy(Val),
    /* pop a1? ; pop a2? */
    Eq(bool, bool, Val, Val),
    Gt(bool, bool, Val, Val),
    /* is conditional? ; what condition? */
    Skip(bool, bool, i16),
    /* is conditional? ; what condition? ; pop result? */
    Call(bool, bool, bool, Val),
    /* pop a1? ; pop a2 */
    Add(bool, bool, Val, Val),
    Mul(bool, bool, Val, Val),
    Div(bool, bool, Val, Val),
    Mod(bool, bool, Val, Val),
    Pow(bool, bool, Val, Val),
    And(bool, bool, Val, Val),
    Or(bool, bool, Val, Val),
    /* pop a1? */
    Not(bool, Val),
    /* ... */
    Pop(Stkval),
    /* pop a2? */
    Write(bool, Stkval, Val),
    /* ... */
    Return(Val),
}

/// Value on fake stack.
enum FSValue {
    Var(Ident),
    Any,
}

/// Build scope of a block.
///
/// Keeps track of where on the stack a block started,
/// so it can all be cleared once the block is left
struct BlockBuild {}

/// A stack that keeps track of values during translation.
/// (Local or shared)
struct FakeStack<'a> {
    values: Vec<FSValue>,
    parent: Option<&'a FakeStack<'a>>,
}
impl<'a> Stack<'a> for FakeStack<'a> {
    type Value = FSValue;
    type Input = Ident;
    type Output = usize;

    fn with_parent(parent: Option<&'a Self>) -> Self {
        Self {
            values: Vec::new(),
            parent,
        }
    }
    fn parent(&self) -> Option<&'a Self> {
        self.parent
    }
    fn values(&self) -> &Vec<Self::Value> {
        &self.values
    }
    fn values_mut(&mut self) -> &mut Vec<Self::Value> {
        &mut self.values
    }

    fn find_map(index: usize, value: &Self::Value, input: &Self::Input) -> Option<Self::Output> {
        matches!(value, FSValue::Var(x) if x == input).then_some(index)
    }
}
impl FakeStack<'_> {
    fn get(&mut self, id: Ident) -> (usize, u16) {
        self.find(&id).unwrap_or_else(|| {
            let i = self.values().len();
            self.push(FSValue::Var(id));
            (i, 0)
        })
    }
}

/// Build scope of a function.
///
/// * Starts with a base block scope that includes any arguments passed
/// * Contains the compiled instructions of all blocks inside it
/// * Keeps track of its own shared stack (contains vars that higher functions access)
/// * Keeps track of its own local stack (vars only used locally)
struct FuncBuild<'a> {
    insts: Vec<Inst>,
    shared: FakeStack<'a>,
    local: FakeStack<'a>,
}

impl<'a> FuncBuild<'a> {
    fn new_root() -> Self {
        FuncBuild {
            insts: Vec::new(),
            shared: FakeStack::with_parent(None),
            local: FakeStack::with_parent(None),
        }
    }
    fn with_parent(parent: &'a FuncBuild<'a>) -> Self {
        FuncBuild {
            insts: Vec::new(),
            shared: FakeStack::with_parent(Some(&parent.shared)),
            local: FakeStack::with_parent(None),
        }
    }

    fn translate(&mut self, bb: &mut BlockBuild, e: &Expr) {
        match e {
            _ => unimplemented!(),
        }
    }
}

pub fn analysis_demo(e: &mut Expr) {
    // analysis pass
    let fs = FuncStat::default();
    let mut scope = Scope::with_parent(None);
    analyze(&fs, &mut scope, e);
}
pub fn translation_demo(e: &Expr) {
    // translation pass
    let mut fb = FuncBuild::new_root();
    let mut bb = BlockBuild {};
    fb.translate(&mut bb, e);
}

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
    fn assigned(&mut self, id: Ident) -> Rc<RefMeta> {
        let Some((rm, _)) = self.find(&id) else {
            let rm: Rc<RefMeta> = Rc::default();
            self.push((id, rm.clone()));
            return rm;
        };
        rm
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
    pub meta: Rc<RefMeta>,
}

#[derive(Debug, Default)]
pub struct RefMeta {
    pub total: Cell<u16>,
    pub is_shared: Cell<bool>,
}

fn analyze(fs: &FuncStat, scope: &mut Scope, e: &mut Expr) {
    match e {
        Expr::Assign(a, b) => {
            let Expr::Literal(Literal::Ident(id, ref_stat)) = &mut **a else {
                panic!("invalid assignment");
            };

            // add to scope
            let rm = scope.assigned(id.clone());

            // add ref stat
            *ref_stat = Some(RefStat { now: rm.total.get(), meta: rm });

            // analyse the value
            analyze(fs, scope, b);
        }
        Expr::Literal(Literal::Ident(id, ref_stat)) => {
            // lookup literal
            let Some((rm, up_levels)) = scope.find(id) else {
                panic!("unfound variable")
            };
            // increment # of uses
            rm.total.update(|c| c + 1);
            // set ref meta
            *ref_stat = Some(RefStat {
                now: rm.total.get(),
                meta: rm.clone(),
            });
            // if we used something external to this scope, note it
            if up_levels != 0 {
                fs.is_unreturnable.set(true);
                rm.is_shared.set(true);
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
#[derive(Debug, PartialEq, Eq)]
pub enum Stkval {
    /* 4b blank ; 8b offset */
    Local(u8),
    /* 4b up levels ; 8b offset */
    Shared(u8, u8),
}
/* 3b type tag */
#[derive(Debug)]
pub enum Val {
    Stack(Stkval, bool),
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
#[derive(Debug)]
pub enum Inst {
    /* ... */
    Copy(Val),
    /* pop a1? ; pop a2? */
    Eq(bool, bool, Val, Val),
    Gt(bool, bool, Val, Val),
    GtEq(bool, bool, Val, Val),
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
#[derive(Debug)]
enum FSValue {
    Var(Ident),
    Any,
}

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

    fn find(&mut self, id: &Ident) -> Stkval {
        self.shared
            .find(id)
            .map(|(count, up_levels)| Stkval::Shared(up_levels as u8, count as u8))
            .or_else(|| self.local.find(id).map(|(c, _)| Stkval::Local(c as u8)))
            .unwrap()
    }

    /// Returns stackval for top item of stack.
    /// (Panics if empty)
    fn top(&self) -> Stkval {
        Stkval::Local(self.local.top_index() as u8)
    }

    /// Pushes a value to stack and returns its stackval.
    fn push_any(&mut self) -> Stkval {
        self.local.push(FSValue::Any);
        self.top()
    }

    /// Pops top stack value and returns its stackval.
    fn pop_top(&mut self) -> Stkval {
        let to_pop = self.top();
        self.local.values_mut().pop();
        to_pop
    }

    fn check_drop(&mut self, v: &Val) -> bool {
        if let Val::Stack(Stkval::Local(i), true) = v {
            self.local.pop(*i as usize);
            true
        } else {
            false
        }
    }
    fn check_drop2(&mut self, v1: &Val, v2: &Val) -> (bool, bool) {
        (self.check_drop(v1), self.check_drop(v2))
    }

    fn gen_unop(&mut self, r: Expr, f: impl Fn(bool, Val) -> Inst) -> Val {
        let v1 = self.translate(r);
        let a1 = self.check_drop(&v1);

        self.insts.push(f(a1, v1));
        Val::Stack(self.push_any(), true)
    }
    fn gen_binop(&mut self, l: Expr, r: Expr, f: impl Fn(bool, bool, Val, Val) -> Inst) -> Val {
        let (v1, v2) = (self.translate(l), self.translate(r));
        let (a1, a2) = self.check_drop2(&v1, &v2);

        self.insts.push(f(a1, a2, v1, v2));
        Val::Stack(self.push_any(), true)
    }

    fn translate(&mut self, e: Expr) -> Val {
        match e {
            /* organisational */
            Expr::Block(mut b) => {
                let last = b.exprs.pop();
                for e in b.exprs {
                    self.translate(e);
                }
                // yield last expr
                last.map_or(Val::Nil, |e| self.translate(e))
            }

            /* 1 to 1 literals */
            Expr::Literal(Literal::Boolean(b)) => Val::Bool(b),
            Expr::Literal(Literal::Float(f)) => Val::Float64(f),
            Expr::Literal(Literal::Integer(i)) => Val::Int64(i),
            Expr::Literal(Literal::Nil) => Val::Nil,
            Expr::Literal(Literal::String(s)) => Val::String(s),

            /* vars */
            Expr::Literal(Literal::Ident(id, Some(rs))) => {
                Val::Stack(self.find(&id), rs.now == rs.meta.total.get())
            }
            Expr::Assign(l, r) => {
                let Expr::Literal(Literal::Ident(id, Some(ref_stat))) = *l else {
                    unreachable!()
                };

                // will the var ever get used?
                let gets_used = ref_stat.now != ref_stat.meta.total.get();

                match *r {
                    // the var's value is a literal, and it gets used; add to stack
                    Expr::Literal(lit) if gets_used => {
                        let v1 = self.translate(Expr::Literal(lit));
                        self.insts.push(Inst::Copy(v1));
                        Val::Stack(self.push_any(), false)
                    }
                    // it doesn't get used; just treat it like a literal
                    Expr::Literal(lit) => self.translate(Expr::Literal(lit)),

                    // value is an expr
                    e => {
                        // translate the value
                        let val = self.translate(e);
                        // we know the top of the stack is the result
                        // so lets keep track of that
                        self.local.swap_top(FSValue::Var(id));
                        // also if this val is never used again, just pop it now
                        if !gets_used {
                            let v1 = self.pop_top();
                            self.insts.push(Inst::Pop(v1));
                        }
                        val
                    }
                }
            }

            /* math */
            Expr::Add(l, r) => self.gen_binop(*l, *r, Inst::Add),
            Expr::Multiply(l, r) => self.gen_binop(*l, *r, Inst::Mul),
            Expr::Divide(l, r) => self.gen_binop(*l, *r, Inst::Div),
            Expr::Modulo(l, r) => self.gen_binop(*l, *r, Inst::Mod),
            Expr::Exponent(l, r) => self.gen_binop(*l, *r, Inst::Pow),

            Expr::Subtract(l, r) => {
                // negate
                let nv2 = match *r {
                    // statically
                    Expr::Literal(Literal::Integer(i)) => Val::Int64(-i),
                    Expr::Literal(Literal::Float(f)) => Val::Float64(-f),
                    // at runtime
                    e => {
                        let v2 = self.translate(e);
                        let a2 = self.check_drop(&v2);
                        self.insts.push(Inst::Mul(a2, false, v2, Val::Int64(-1)));
                        Val::Stack(self.pop_top(), true)
                    }
                };

                // add
                let v1 = self.translate(*l);
                let a1 = self.check_drop(&v1);

                self.insts.push(Inst::Add(a1, true, v1, nv2));
                Val::Stack(self.push_any(), true)
            }

            /* logic */
            Expr::And(l, r) => self.gen_binop(*l, *r, Inst::And),
            Expr::Or(l, r) => self.gen_binop(*l, *r, Inst::Or),
            Expr::EqualTo(l, r) => self.gen_binop(*l, *r, Inst::Eq),
            Expr::GreaterThan(l, r) => self.gen_binop(*l, *r, Inst::Gt),
            Expr::GreaterThanOrEqualTo(l, r) => self.gen_binop(*l, *r, Inst::GtEq),
            Expr::Not(r) => self.gen_unop(*r, Inst::Not),

            Expr::NotEqualTo(l, r) => self.translate(Expr::Not(Box::new(Expr::EqualTo(l, r)))),
            Expr::LessThan(l, r) => self.translate(Expr::GreaterThan(r, l)),

            e => unimplemented!("{e:?}"),
        }
    }
}

pub fn analysis_demo(e: &mut Expr) {
    // analysis pass
    let fs = FuncStat::default();
    let mut scope = Scope::with_parent(None);
    analyze(&fs, &mut scope, e);
}
pub fn translation_demo(e: Expr) -> Vec<Inst> {
    // translation pass
    let mut fb = FuncBuild::new_root();
    fb.translate(e);
    fb.insts
}

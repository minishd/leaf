use std::{cell::Cell, rc::Rc};

use crate::{
    lexer::{Ident, Literal},
    parser::Expr,
};

pub type FuncMeta = Rc<Cell<bool>>;

#[derive(Debug, Clone)]
pub struct RefMeta {
    pub now: u16,
    pub total: Rc<Cell<u16>>,
}

struct Scope<'a> {
    idents: Vec<(Ident, Rc<Cell<u16>>)>,
    parent: Option<&'a Scope<'a>>,
}
impl<'a> Scope<'a> {
    fn with_parent(parent: Option<&'a Scope>) -> Self {
        Scope {
            idents: Vec::new(),
            parent,
        }
    }
    fn assigned(&mut self, id: Ident) {
        self.idents.push((id, Rc::default()));
    }
    fn find(&self, id: &Ident) -> (Rc<Cell<u16>>, bool) {
        let mut cur = Some(self);
        let mut went_up = false;
        while let Some(scope) = cur {
            let Some((_, count)) = scope.idents.iter().rev().find(|i| i.0 == *id) else {
                cur = scope.parent;
                went_up = true;
                continue;
            };
            return (count.clone(), went_up);
        }
        panic!("undefined variable");
    }
}

pub fn compile(e: &mut Expr) {
    let fm = FuncMeta::default();
    let mut scope = Scope::with_parent(None);
    analyze(&fm, &mut scope, e);
}

fn analyze(fm: &FuncMeta, scope: &mut Scope, e: &mut Expr) {
    match e {
        Expr::Assign(a, b) => {
            let Expr::Literal(Literal::Ident(id, _)) = &**a else {
                panic!("invalid assignment");
            };

            // add to scope
            scope.assigned(id.clone());

            // analyse the value
            analyze(fm, scope, b);
        }
        Expr::Literal(Literal::Ident(id, ref_meta)) => {
            // lookup literal
            let (count, went_up) = scope.find(id);
            // increment # of uses
            count.update(|c| c + 1);
            // set ref meta
            *ref_meta = Some(RefMeta {
                now: count.get(),
                total: count,
            });
            // if we used something external to this scope, note it
            if went_up {
                fm.set(true);
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
                analyze(fm, &mut scope, e);
            }
        }
        Expr::Func(a, b, func_meta) => {
            // new function new context
            let fm = FuncMeta::default();
            *func_meta = Some(fm.clone());
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
            analyze(&fm, &mut scope, b);
        }
        Expr::If(a, b, c) => {
            analyze(fm, scope, a);
            analyze(fm, scope, b);
            if let Some(c) = c {
                analyze(fm, scope, c);
            }
        }
        Expr::Call(a, b) => {
            analyze(fm, scope, a);
            for e in b {
                analyze(fm, scope, e);
            }
        }
        Expr::Return(a) | Expr::Negate(a) | Expr::Not(a) => analyze(fm, scope, a),
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
        | Expr::AddAssign(a, b)
        | Expr::SubtractAssign(a, b)
        | Expr::MultiplyAssign(a, b)
        | Expr::DivideAssign(a, b) => {
            analyze(fm, scope, a);
            analyze(fm, scope, b);
        }
    }
}

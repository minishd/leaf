use std::cell::Cell;

use crate::{
    lexer::{Ident, Literal},
    parser::Expr,
};

struct Scope<'a> {
    idents: Vec<(Ident, Cell<u16>)>,
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
        self.idents.push((id, Cell::default()));
    }
}

pub fn compile(mut e: Expr) {
    let mut scope = Scope::with_parent(None);
    analyze(&mut scope, &mut e);
}

fn analyze(scope: &mut Scope, e: &mut Expr) {
    match e {
        Expr::Assign(a, b) => {
            let Expr::Literal(Literal::Ident(id), _) = &**a else {
                panic!("invalid assignment");
            };

            // add to scope
            scope.assigned(id.clone());

            // analyse the value
            analyze(scope, b);
        }
        Expr::Literal(Literal::Ident(id), _) => {
            // lookup literal
            let mut cur = &*scope;
            loop {
                let Some((_, count)) = cur.idents.iter().find(|i| i.0 == *id) else {
                    if let Some(parent) = cur.parent {
                        cur = parent;
                    } else {
                        panic!("undefined variable");
                    }
                    continue;
                };
                count.update(|c| c + 1);
                let count = count.get();
                println!("ref {id} #{count}");
                break;
            }
        }
        // ignore
        Expr::Literal(_, _) => {}
        // for recursion..
        Expr::Block(a) => {
            // blocks have their own scope
            let mut scope2 = Scope::with_parent(Some(scope));
            // analyze the contents in the new scope
            for e in &mut a.exprs {
                analyze(&mut scope2, e);
            }
        }
        Expr::Func(a, b) => {
            // functions have their own scope, because they have args
            let mut scope = Scope::with_parent(Some(scope));

            // init args
            for e in a {
                let Expr::Literal(Literal::Ident(id), _) = e else {
                    panic!("invalid arg def");
                };
                scope.assigned(id.clone());
            }

            // now analyze the body in the new scope
            analyze(&mut scope, b);
        }
        Expr::If(a, b, c) => {
            analyze(scope, a);
            analyze(scope, b);
            if let Some(c) = c {
                analyze(scope, c);
            }
        }
        Expr::Call(a, b) => {
            analyze(scope, a);
            for e in b {
                analyze(scope, e);
            }
        }
        Expr::Return(a) | Expr::Negate(a) | Expr::Not(a) => analyze(scope, a),
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
            analyze(scope, a);
            analyze(scope, b);
        }
    }
}

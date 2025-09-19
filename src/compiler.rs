use crate::{
    lexer::{Ident, Literal},
    parser::Expr,
};

type ScopeIdx = usize;

#[derive(Default, Debug)]
struct AnalysisScope {
    prev: Option<ScopeIdx>,
    next: Vec<ScopeIdx>,
    idents: Vec<(Ident, u16)>,
}

impl AnalysisScope {
    pub fn new() -> Self {
        Self::default()
    }

    fn initialize(&mut self, id: Ident) {
        self.idents.push((id, 0));
    }
}

#[derive(Default, Debug)]
pub struct Analyzer {
    scopes: Vec<AnalysisScope>,
}
impl Analyzer {
    pub fn new() -> Self {
        Self::default()
    }

    fn new_scope(&mut self) {
        // Get indices
        let cur_idx = self.scopes.len().checked_sub(1);
        let next_idx = cur_idx.map(|i| i + 1).unwrap_or_default();

        // If we have a current scope, add in the new scope's index
        if let Some(Some(cur_scope)) = cur_idx.map(|i| self.scopes.get_mut(i)) {
            cur_scope.next.push(next_idx);
        }

        // Create new scope
        self.scopes.push(AnalysisScope::new());
        let next_scope = self.scopes.get_mut(next_idx).unwrap();
        // Tie it back to the current scope (if there is one)
        next_scope.prev = cur_idx;
    }

    fn get_scope(&mut self) -> &mut AnalysisScope {
        self.scopes.last_mut().unwrap()
    }

    fn increment_ident(&mut self, id: &Ident) -> Option<u16> {
        let mut cur = self.get_scope();

        loop {
            // Try to find it
            if let Some(count) = cur
                .idents
                .iter_mut()
                .rev()
                .find(|(i, _)| id == i)
                .map(|(_, c)| c)
            {
                let old_count = *count;
                *count += 1;
                return Some(old_count);
            }
            // Didn't find it so try scope above
            if let Some(prev) = cur.prev {
                cur = self.scopes.get_mut(prev).unwrap();
            } else {
                // No more scopes above
                return None;
            }
        }
    }

    pub fn analyze(&mut self, e: Expr) {
        match e {
            Expr::Assign(a, b) => {
                let Expr::Literal(Literal::Ident(id)) = *a else {
                    panic!("invalid assignment");
                };

                // make new counter for ident in our scope
                let sc = self.get_scope();
                sc.initialize(id);

                // analyse the value
                self.analyze(*b);
            }
            Expr::Literal(Literal::Ident(id)) => {
                // lookup literal
                self.increment_ident(&id).expect("not found var");
            }
            // ignore
            Expr::Literal(_) => {}
            // for recursion..
            Expr::Block(a) => {
                // blocks have their own scope
                self.new_scope();
                // analyze the contents in the new scope
                for e in a.exprs {
                    self.analyze(e);
                }
            }
            Expr::Func(a, b) => {
                // functions have their own scope, because they have args
                self.new_scope();
                let new_sc = self.get_scope();

                for e in a {
                    let Expr::Literal(Literal::Ident(id)) = e else {
                        panic!("invalid arg def");
                    };
                    new_sc.initialize(id);
                }

                // now analyze the body in the new scope
                self.analyze(*b);
            }
            Expr::If(a, b, c) => {
                self.analyze(*a);
                self.analyze(*b);
                if let Some(c) = c {
                    self.analyze(*c);
                }
            }
            Expr::Call(a, b) => {
                self.analyze(*a);
                for e in b {
                    self.analyze(e);
                }
            }
            Expr::Return(a) | Expr::Negate(a) | Expr::Not(a) => self.analyze(*a),
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
                self.analyze(*a);
                self.analyze(*b);
            }
        }
    }
}

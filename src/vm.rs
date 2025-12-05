use std::cell::RefCell;

use crate::compiler::{Inst, Stkval, Val};

#[derive(Default)]
struct FuncVm<'a> {
    depth: usize,
    parent_vm: Option<&'a FuncVm<'a>>,
    shared: RefCell<Vec<Val>>,
    locals: Vec<Val>,
}
impl<'a> FuncVm<'a> {
    fn with(parent_vm: &'a FuncVm<'a>, locals: Vec<Val>) -> Self {
        Self {
            parent_vm: Some(parent_vm),
            depth: parent_vm.depth + 1,
            shared: RefCell::default(),
            locals,
        }
    }

    fn get(&mut self, v: &Val) -> Val {
        use {Stkval::*, Val::*};

        match v {
            Stack(Local(o), true) => self.locals.remove(*o as usize),
            Stack(Local(o), false) => self.locals[*o as usize].clone(),

            Stack(Shared(l, o), false) => {
                let mut vm = &*self;
                for _ in 0..*l {
                    vm = vm.parent_vm.as_ref().unwrap();
                }
                vm.shared.borrow_mut()[*o as usize].clone()
            }
            Stack(Shared(_, _), true) => panic!("not allowed"),

            v => v.clone(),
        }
    }

    fn eval_all(&mut self, insts: &[Inst]) -> Val {
        use {Inst::*, Val::*};

        let mut pc = 0;
        while pc < insts.len() {
            let inst = &insts[pc];

            match inst {
                /* rhs */
                Not(a) | Return(a) => {
                    let r = match (inst, self.get(a)) {
                        (Not(_), Bool(a)) => Bool(!a),
                        (Return(_), Func(true, _, _)) => panic!("func is unreturnable"),
                        (Return(_), a) => return a,
                        _ => unimplemented!(),
                    };
                    self.locals.push(r);
                }
                /* s rhs */
                Copy(s, a) => {
                    let a = self.get(a);
                    if !*s {
                        self.locals.push(a);
                    } else {
                        self.shared.borrow_mut().push(a);
                    }
                }
                /* k rhs */
                Call(k, n, a) => {
                    // check validity
                    let a = self.get(a);
                    let Func(_, arity, insts) = a else {
                        panic!("called non-function {a:?}")
                    };

                    // collect args from stack :)
                    let args_start = self.locals.len() - *n as usize;
                    let args = self.locals.split_off(args_start);

                    // make sure its the right amount
                    if *n != arity {
                        panic!("wrong # args")
                    }

                    // exec
                    let mut vm = FuncVm::with(self, args);
                    let r = vm.eval_all(&insts);
                    // push value if were supposed to
                    if *k {
                        self.locals.push(r);
                    }
                }

                /* sv rhs */
                Move(sv, a) => {
                    let a = self.get(a);
                    match sv {
                        Stkval::Local(o) => {
                            self.locals[*o as usize] = a;
                        }
                        Stkval::Shared(l, o) => {
                            let mut vm = &*self;
                            for _ in 0..*l {
                                vm = vm.parent_vm.unwrap();
                            }
                            vm.shared.borrow_mut()[*o as usize] = a;
                        }
                    }
                }

                /* lhs rhs */
                Eq(a, b)
                | Gt(a, b)
                | GtEq(a, b)
                | Add(a, b)
                | Mul(a, b)
                | Div(a, b)
                | Mod(a, b)
                | Pow(a, b)
                | And(a, b)
                | Or(a, b) => {
                    let r = match (inst, self.get(a), self.get(b)) {
                        (Add(_, _), Int64(a), Int64(b)) => Int64(a + b),
                        (Mul(_, _), Int64(a), Int64(b)) => Int64(a * b),
                        (Div(_, _), Int64(a), Int64(b)) => Int64(a / b),
                        (Mod(_, _), Int64(a), Int64(b)) => Int64(a % b),
                        (Pow(_, _), Int64(a), Int64(b)) => Int64(a.pow(b.try_into().unwrap())),
                        (And(_, _), Bool(a), Bool(b)) => Bool(a && b),
                        (Or(_, _), Bool(a), Bool(b)) => Bool(a || b),
                        (Eq(_, _), a, b) => Bool(a == b),
                        (Gt(_, _), Int64(a), Int64(b)) => Bool(a > b),
                        (GtEq(_, _), Int64(a), Int64(b)) => Bool(a >= b),

                        x => unimplemented!("{x:?}"),
                    };
                    self.locals.push(r);
                }

                _ => unimplemented!(),
            }

            pc += 1;
        }

        Nil
    }
}

pub fn run(insts: &[Inst]) -> Val {
    let mut vm = FuncVm::default();
    vm.eval_all(insts)
}

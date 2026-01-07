use std::collections::{HashMap, VecDeque};

use crate::ring_field::Ring;
use super::function::*;

pub struct Equation<TEntry: Ring> {
    lhs: Function<TEntry>,
    rhs: Function<TEntry>,
}

impl<TEntry: Ring> Equation<TEntry> {
    pub fn new(lhs: Function<TEntry>, rhs: Function<TEntry>) -> Self {
        Self { lhs, rhs }
    }

    pub fn solve_for(&self, var: &str) -> Option<Function<TEntry>> {
        let neg1 = Box::new(Function::Constant(TEntry::multiplicative_ident().negate()));
        let (mut lh, mut rh) = (self.lhs.clone(), self.rhs.clone());
        let mut path = match (lh.find(var), rh.find(var)) {
            (None, None) => panic!("No variable named {var} found"),
            (None, Some(path)) => {
                std::mem::swap(&mut lh, &mut rh);
                path
            }
            (Some(path), None) => path,
            (Some(_), Some(_)) => panic!("Can't yet handle multiple of the same variable"),
        }
        .into_iter()
        .collect::<VecDeque<_>>();
        while let Some(next_op) = path.pop_back() {
            match (lh, next_op) {
                (Function::Constant(_) | Function::Variable(_) | Function::Undefined, _) => {
                    unreachable!("Path should have ended by now")
                }
                (Function::Sum(lhs, rhs), FunctionPath::Left) => {
                    lh = *lhs;
                    rh =
                        Function::Sum(Box::new(rh), Box::new(Function::Product(neg1.clone(), rhs)));
                }
                (Function::Sum(lhs, rhs), FunctionPath::Right) => {
                    lh = *rhs;
                    rh =
                        Function::Sum(Box::new(rh), Box::new(Function::Product(neg1.clone(), lhs)));
                }
                (Function::Product(lhs, rhs), FunctionPath::Left) => {
                    lh = *lhs;
                    rh = Function::Product(Box::new(rh), Box::new(Function::Inverse(rhs)));
                }
                (Function::Product(lhs, rhs), FunctionPath::Right) => {
                    lh = *rhs;
                    rh = Function::Product(Box::new(Function::Inverse(lhs)), Box::new(rh));
                }
                (Function::Inverse(val), _) => {
                    lh = *val;
                    rh = Function::Inverse(Box::new(rh));
                }
            }
            let rh_test = rh.eval(&HashMap::new());
            if let Function::Undefined = rh_test {
                return None;
            } else {
                rh = rh_test;
            }
        }
        assert_eq!(lh, Function::Variable(var.to_string()));
        Some(rh)
    }

    pub fn equals_zero(&self) -> bool {
        self.rhs == Function::Constant(TEntry::additive_ident())
    }
}

impl<TEntry: Ring> std::fmt::Debug for Equation<TEntry> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} = {:?}", self.lhs, self.rhs)
    }
}

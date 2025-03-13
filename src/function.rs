use std::{
    collections::{HashMap, VecDeque},
    usize,
};

use crate::ring_field::Ring;

#[derive(Clone, PartialEq, Eq)]
pub enum Function<TEntry: Ring> {
    Constant(TEntry),
    Variable(String),
    Sum(Box<Function<TEntry>>, Box<Function<TEntry>>),
    Product(Box<Function<TEntry>>, Box<Function<TEntry>>),
    Inverse(Box<Function<TEntry>>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum EvalResult<TEntry: Ring> {
    Res(Function<TEntry>),
    InvalidCalculation,
}
use EvalResult::*;

#[derive(Debug)]
enum FunctionPath {
    Left,
    Right,
}

impl<TEntry: Ring> Function<TEntry> {
    pub fn eval(&self, vars: &HashMap<String, Function<TEntry>>) -> EvalResult<TEntry> {
        match self {
            Self::Constant(_) => Res(self.clone()),
            Self::Variable(v) => vars
                .get(v)
                .map(|n| Res(n.clone()))
                .unwrap_or(Res(self.clone())),

            Self::Sum(lhs, rhs) | Self::Product(lhs, rhs) => {
                let (lv, rv) = (lhs.eval(vars), rhs.eval(vars));
                match self {
                    // Additive identity
                    Self::Sum(_, _) if lv == Res(Self::Constant(TEntry::additive_ident())) => {
                        return rv
                    }
                    Self::Sum(_, _) if rv == Res(Self::Constant(TEntry::additive_ident())) => {
                        return lv
                    }

                    // Multiplicative identity
                    Self::Product(_, _)
                        if lv == Res(Self::Constant(TEntry::multiplicative_ident())) =>
                    {
                        return rv
                    }
                    Self::Product(_, _)
                        if rv == Res(Self::Constant(TEntry::multiplicative_ident())) =>
                    {
                        return lv
                    }

                    // Multiplicative absorption (0)
                    Self::Product(_, _)
                        if lv == Res(Self::Constant(TEntry::additive_ident()))
                            || rv == Res(Self::Constant(TEntry::additive_ident())) =>
                    {
                        return Res(Self::Constant(TEntry::additive_ident()))
                    }
                    _ => {}
                }
                match (lv, rv) {
                    (Res(Self::Constant(lhs_val)), Res(Self::Constant(rhs_val))) => match self {
                        Self::Sum(_, _) => Res(Self::Constant(lhs_val + rhs_val)),
                        Self::Product(_, _) => Res(Self::Constant(lhs_val * rhs_val)),
                        _ => unreachable!(),
                    },
                    (Res(rhs), Res(lhs)) => Res(match self {
                        Self::Sum(_, _) => Self::Sum(Box::new(rhs), Box::new(lhs)),
                        Self::Product(_, _) => Self::Product(Box::new(rhs), Box::new(lhs)),
                        _ => unreachable!(),
                    }),
                    (Res(_), InvalidCalculation) => InvalidCalculation,
                    (InvalidCalculation, Res(_)) => InvalidCalculation,
                    (InvalidCalculation, InvalidCalculation) => InvalidCalculation,
                }
            }
            Self::Inverse(val) => match val.eval(vars) {
                Res(Self::Constant(c)) => c
                    .try_inverse()
                    .map(|v| Res(Function::Constant(v)))
                    .unwrap_or(InvalidCalculation),
                Res(function) => Res(Self::Inverse(Box::new(function))),
                InvalidCalculation => InvalidCalculation,
            },
        }
    }

    fn find(&self, var: &str) -> Option<Vec<FunctionPath>> {
        match self {
            Self::Constant(_) => None,
            Self::Variable(v) => {
                if v == var {
                    Some(vec![])
                } else {
                    None
                }
            }
            Self::Sum(lhs, rhs) | Self::Product(lhs, rhs) => match (lhs.find(var), rhs.find(var)) {
                (None, None) => None,
                (None, Some(mut path)) => {
                    path.push(FunctionPath::Right);
                    Some(path)
                }
                (Some(mut path), None) => {
                    path.push(FunctionPath::Left);
                    Some(path)
                }
                (Some(_), Some(_)) => panic!("Can't yet handle multiple of the same variable"),
            },
            Self::Inverse(val) => {
                if let Some(mut path) = val.find(var) {
                    path.push(FunctionPath::Left);
                    Some(path)
                } else {
                    None
                }
            }
        }
    }

    fn priority(&self) -> usize {
        match self {
            Self::Sum(_, _) => 3,
            Self::Product(_, _) => 2,
            Self::Inverse(_) => 1,
            Self::Constant(_) | Self::Variable(_) => 0,
        }
    }

    fn debug(&self, priority: usize) -> String {
        let mut string = String::with_capacity(512);
        if self.priority() > priority {
            string.push('(');
        }
        match self {
            Function::Constant(c) => string.push_str(&format!("{c:?}")),
            Function::Variable(v) => string.push_str(v),
            Function::Sum(lhs, rhs) | Function::Product(lhs, rhs) => {
                string.push_str(&lhs.debug(self.priority()));
                string.push(match self {
                    Function::Sum(_, _) => '+',
                    Function::Product(_, _) => '*',
                    _ => unreachable!(),
                });
                string.push_str(&rhs.debug(self.priority()));
            }
            Function::Inverse(val) => {
                string.push_str(&val.debug(self.priority()));
                string.push_str("⁻¹");
            }
        }
        if self.priority() > priority {
            string.push(')');
        }
        string
    }
}

impl<TEntry: Ring> std::fmt::Debug for Function<TEntry> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.debug(usize::MAX))
    }
}

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
        let mut path = match (lh.find(&var), rh.find(&var)) {
            (None, None) => panic!("No variable named {var} found"),
            (None, Some(path)) => {
                let tmp = lh;
                lh = rh;
                rh = tmp;
                path
            }
            (Some(path), None) => path,
            (Some(_), Some(_)) => panic!("Can't yet handle multiple of the same variable"),
        }
        .into_iter()
        .collect::<VecDeque<_>>();
        println!("Path found is {path:?}");
        while let Some(next_op) = path.pop_back() {
            println!("Curr equation: {lh:?} = {rh:?}, next op: {next_op:?}");
            match (lh, next_op) {
                (Function::Constant(_), _) | (Function::Variable(_), _) => {
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
            if let Res(rh_new) = rh_test {
                rh = rh_new;
            } else {
                return None;
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

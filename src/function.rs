use std::{
    collections::{HashMap, VecDeque},
    usize,
};

const ALWAYS_BRACKET: bool = false;

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

#[derive(Debug, PartialEq, Eq)]
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
                println!("Inspecting {lv:?}, {rv:?}");
                match (&lv, &rv) {
                    (Res(lhs), Res(rhs)) => {
                        println!(
                            "{:?}",
                            rhs.is_const_product() == Some(FunctionPath::Left) && lhs.is_constant()
                        );
                    },
                    _ => {}
                }

                match (lv, rv) {
                    (Res(Self::Constant(lhs_val)), Res(Self::Constant(rhs_val))) => match self {
                        Self::Sum(_, _) => Res(Self::Constant(lhs_val + rhs_val)),
                        Self::Product(_, _) => Res(Self::Constant(lhs_val * rhs_val)),
                        _ => unreachable!(),
                    },
                    (Res(lhs), Res(rhs)) => Res(match self {
                        Self::Sum(_, _) => {
                            // Additive identity
                            if lhs == Self::Constant(TEntry::additive_ident()) {
                                rhs
                            } else if rhs == Self::Constant(TEntry::additive_ident()) {
                                lhs
                            // Additive associativity
                            } else if lhs.is_constant() && rhs.is_const_sum() {
                                match rhs {
                                    Self::Sum(a, b) if a.is_constant() => Self::Sum(
                                        Box::new(Self::Constant(
                                            a.as_constant() + lhs.as_constant(),
                                        )),
                                        b,
                                    ),
                                    _ => unreachable!(),
                                }
                            } else if lhs.is_const_sum() && rhs.is_constant() {
                                match lhs {
                                    Self::Sum(a, b) if a.is_constant() => Self::Sum(
                                        Box::new(Self::Constant(
                                            a.as_constant() + rhs.as_constant(),
                                        )),
                                        b,
                                    ),
                                    _ => unreachable!(),
                                }
                            } else {
                                Self::Sum(Box::new(lhs), Box::new(rhs))
                            }
                        }
                        Self::Product(_, _) => {
                            println!("Entered product");
                            // Multiplicative identity
                            if lhs == Self::Constant(TEntry::multiplicative_ident()) {
                                rhs
                            } else if rhs == Self::Constant(TEntry::multiplicative_ident()) {
                                lhs
                            // Multiplicative absorption (0)
                            } else if lhs == Self::Constant(TEntry::additive_ident())
                                || rhs == Self::Constant(TEntry::additive_ident())
                            {
                                Self::Constant(TEntry::additive_ident())
                            // Multiplicative absorption
                            } else if lhs.is_const_product() == Some(FunctionPath::Right)
                                && rhs.is_constant()
                            {
                                match lhs {
                                    Self::Product(a, b) => Self::Product(
                                        a,
                                        Box::new(Self::Constant(
                                            b.as_constant() * rhs.as_constant(),
                                        )),
                                    ),
                                    _ => unreachable!(),
                                }
                            } else if rhs.is_const_product() == Some(FunctionPath::Left)
                                && lhs.is_constant()
                            {
                                println!("Entered function");
                                match rhs {
                                    Self::Product(a, b) => Self::Product(
                                        Box::new(Self::Constant(
                                            lhs.as_constant() * a.as_constant(),
                                        )),
                                        b,
                                    ),
                                    _ => unreachable!(),
                                }
                            } else {
                                println!("Did nothing");
                                Self::Product(Box::new(lhs), Box::new(rhs))
                            }
                        }
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

    fn is_constant(&self) -> bool {
        match self {
            Self::Constant(_) => true,
            _ => false,
        }
    }

    fn as_constant(&self) -> TEntry {
        match self {
            Self::Constant(v) => *v,
            _ => panic!("Value wasn't a constant"),
        }
    }

    fn is_const_product(&self) -> Option<FunctionPath> {
        match self {
            Self::Product(a, _) if a.is_constant() => Some(FunctionPath::Left),
            Self::Product(_, b) if b.is_constant() => Some(FunctionPath::Right),
            _ => None,
        }
    }

    fn is_const_sum(&self) -> bool {
        match self {
            Self::Sum(a, b) if a.is_constant() || b.is_constant() => true,
            _ => false,
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
        if self.priority() > priority || ALWAYS_BRACKET {
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
        if self.priority() > priority || ALWAYS_BRACKET {
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
        while let Some(next_op) = path.pop_back() {
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

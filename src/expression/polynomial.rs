use std::{
    collections::HashMap,
    ops::{Add, Mul, Sub},
};

use crate::ring_field::Ring;

use super::function::Function;

#[derive(Clone, Hash, Debug, PartialEq, Eq)]
pub struct Term<TEntry: Ring>(TEntry, Vec<(String, usize)>);

#[derive(Clone, Hash, Debug, PartialEq, Eq)]
pub struct Polynomial<TEntry: Ring> {
    entries: Vec<Term<TEntry>>,
}

impl<TEntry: Ring> Polynomial<TEntry> {
    pub fn new(prec_entries: Vec<Term<TEntry>>) -> Self {
        let mut entries = vec![];
        for entry in prec_entries {
            if !entries.iter().any(|e| entry.like_term(e)) {
                entries.push(entry);
            }
        }
        Self { entries }
    }
}

impl<TEntry: Ring> Term<TEntry> {
    pub fn new(coeff: TEntry, vars_iter: &mut dyn Iterator<Item = (String, usize)>) -> Self {
        let mut vars: Vec<(String, usize)> = vec![];
        for v in vars_iter {
            if let Some(same) = vars.iter_mut().find(|sv| sv.0 == v.0) {
                same.1 += v.1;
            } else {
                vars.push(v);
            }
        }
        Self(coeff, vars)
    }

    pub fn like_term(&self, other: &Self) -> bool {
        // Make sure other contains everything self does
        for (i, (var, pow)) in self.1.iter().enumerate() {
            if !other.1.iter().any(|(v, p)| v == var && p == pow) {
                return false;
            }
        }
        // As long as it does (and contains no more elements), it's valid
        self.1.len() == other.1.len()
    }

    fn var_to_fn(var: String, pow: usize) -> Function<TEntry> {
        let mut res = Function::Constant(TEntry::multiplicative_ident());
        for _ in 0..pow {
            res = Function::Product(Box::new(Function::Variable(var.clone())), Box::new(res));
        }
        res.eval(&HashMap::new())
    }
}

impl<TEntry: Ring> Into<Function<TEntry>> for Term<TEntry> {
    fn into(self) -> Function<TEntry> {
        let mut var_iter = self.1.into_iter();
        if let Some(first) = var_iter.next() {
            let mut res = Self::var_to_fn(first.0, first.1);
            for next in var_iter {
                res = Function::Sum(Box::new(Self::var_to_fn(next.0, next.1)), Box::new(res))
            }
            res.eval(&HashMap::new())
        } else {
            Function::Constant(self.0)
        }
    }
}

impl<TEntry: Ring> Into<Function<TEntry>> for Polynomial<TEntry> {
    fn into(self) -> Function<TEntry> {
        let mut entry_iter = self.entries.iter();
        if let Some(first) = entry_iter.next() {
            first.clone().into()
        } else {
            Function::Constant(TEntry::additive_ident())
        }
    }
}

impl<TEntry: Ring> Add for Polynomial<TEntry> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let mut new_entries = self.entries;
        for e in rhs.entries {
            new_entries.push(e);
        }
        Self::new(new_entries)
    }
}

impl<TEntry: Ring> Sub for Polynomial<TEntry> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        let mut new_entries = self.entries;
        for e in rhs.entries {
            new_entries.push(Term::new(e.0.negate(), &mut e.1.into_iter()));
        }
        Self::new(new_entries)
    }
}

impl<TEntry: Ring> Mul for Polynomial<TEntry> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut new_entries = vec![];
        for re in rhs.entries {
            for le in self.entries.clone() {
                new_entries.push(Term::new(
                    re.0.clone() * le.0,
                    &mut re.1.clone().into_iter().chain(le.1.into_iter()),
                ))
            }
        }
        Self::new(new_entries)
    }
}

impl<TEntry: Ring> Ring for Polynomial<TEntry> {
    fn try_inverse(&self) -> Option<Self> {
        todo!()
    }

    fn negate(&self) -> Self {
        todo!()
    }

    fn additive_ident() -> Self {
        todo!()
    }

    fn multiplicative_ident() -> Self {
        todo!()
    }

    fn generate(rng: &mut rand::prelude::ThreadRng, basic: bool) -> Self {
        todo!()
    }
}
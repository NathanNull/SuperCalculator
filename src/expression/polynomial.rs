use std::{
    collections::{HashMap, HashSet},
    ops::{Add, Mul, Sub},
};

use rand::Rng;
use reikna::factor::quick_factorize;

use crate::{
    num::rational::{gcf, Rational},
    r,
    ring_field::Ring,
};

use super::function::{Function, VARS};

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Term<TEntry: Ring>(TEntry, Vec<(String, usize)>);

impl<TEntry: Ring> std::fmt::Debug for Term<TEntry> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)?;
        for v in &self.1 {
            write!(f, " {}^{}", v.0, v.1)?;
        }
        Ok(())
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Polynomial<TEntry: Ring> {
    entries: Vec<Term<TEntry>>,
}

impl<TEntry: Ring> std::fmt::Debug for Polynomial<TEntry> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.entries
                .iter()
                .map(|t| format!("{t:?}"))
                .collect::<Vec<_>>()
                .join(" + ")
        )
    }
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

    fn single_var(&self) -> Result<String, ()> {
        let mut var = None;
        for Term(_, vars) in &self.entries {
            for (tvar, _) in vars {
                if var.is_none_or(|v| &v == tvar) {
                    var = Some(tvar.clone())
                } else {
                    return Err(());
                }
            }
        }
        var.ok_or(())
    }

    fn lowest_degree(&self) -> Term<TEntry> {
        if self.entries.len() == 0 {
            return Term::new(TEntry::additive_ident(), vec![]);
        }
        let mut lowest = self.entries[0].clone();
        for t in 1..self.entries.len() {
            if self.entries[t].degree() < lowest.degree() {
                lowest = self.entries[t].clone();
            }
        }
        lowest
    }

    fn highest_degree(&self) -> Term<TEntry> {
        if self.entries.len() == 0 {
            return Term::new(TEntry::additive_ident(), vec![]);
        }
        let mut highest = self.entries[0].clone();
        for t in 1..self.entries.len() {
            if self.entries[t].degree() > highest.degree() {
                highest = self.entries[t].clone();
            }
        }
        highest
    }
}

impl<TEntry: Ring> Term<TEntry> {
    pub fn new(coeff: TEntry, vars_iter: Vec<(String, usize)>) -> Self {
        let mut vars: Vec<(String, usize)> = vec![];
        for v in vars_iter {
            if let Some(same) = vars.iter_mut().find(|sv| sv.0 == v.0) {
                same.1 += v.1;
            } else if v.1 != 0 {
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

    fn degree(&self) -> usize {
        self.1.iter().fold(0, |acc, (_, p)| acc + *p)
    }
}

impl Polynomial<Rational> {
    pub fn zeros(&self) -> Result<Vec<Rational>, ()> {
        let var = self.single_var()?;

        // Using the rational root theorem, i.e. for a polynomial in one variable with
        // integer coefficients, any rational zeros are of the form p/q, with p being a factor
        // of the constant term and q being a factor of the highest degree term.
        let mut gcd = 1;
        for Term(c, _) in &self.entries {
            gcd = gcf(gcd, c.den());
        }
        let gcd_r = Rational::new(true, gcd, 1);
        let l = self.lowest_degree();
        let a0 = (if l.degree() == 0 { l.0 } else { r!(0) } * gcd_r).num();
        let an = (self.highest_degree().0 * gcd_r).num();
        let a0_pf = quick_factorize(a0);
        let an_pf = quick_factorize(an);
        let a0_f: HashSet<u64> = HashSet::from_iter((0..1 << a0_pf.len()).map(|i| {
            a0_pf
                .iter()
                .enumerate()
                .map(|(idx, f)| if i & (1 << idx) == 0 { *f } else { 1 })
                .reduce(|a, b| a * b)
                .unwrap_or(1)
        }));
        let an_f: HashSet<u64> = HashSet::from_iter((0..1 << an_pf.len()).map(|i| {
            an_pf
                .iter()
                .enumerate()
                .map(|(idx, f)| if i & (1 << idx) == 1 { *f } else { 1 })
                .reduce(|a, b| a * b)
                .unwrap_or(1)
        }));

        let mut map = HashMap::new();
        let mut zeros = vec![];
        let mut tmp_f = self.clone();
        'choose: for p in &a0_f {
            for q in &an_f {
                for sign in [true, false] {
                    let v = Rational::new(sign, *p, *q);
                    map.insert(var.clone(), Function::Constant(v));
                    // if f(p/q) = 0
                    println!(
                        "Testing {:?} ({tmp_f:?}) at {:?}",
                        Into::<Function<Rational>>::into(tmp_f.clone()),
                        v
                    );
                    while Into::<Function<Rational>>::into(tmp_f.clone()).eval(&map)
                        == Function::Constant(Rational::additive_ident())
                    {
                        zeros.push(v);
                        tmp_f = tmp_f.synthetic_divide(v)?;
                    }
                    // We've divided out everything and are just left with 1
                    if tmp_f.entries == vec![Term::new(r!(1), vec![])] {
                        break 'choose;
                    }
                }
            }
        }
        Ok(zeros)
    }

    fn synthetic_divide(&self, val: Rational) -> Result<Self, ()> {
        let var = self.single_var()?;
        let max_deg = self.highest_degree().degree();
        let mut carry = 0.into();
        let mut bottom_line = vec![r!(0); max_deg + 1];
        for d in (0..=max_deg).rev() {
            let top = self
                .entries
                .iter()
                .find(|t| t.degree() == d)
                .map(|t| t.0)
                .unwrap_or(0.into());
            carry = top + carry * val;
            bottom_line[d] = carry;
        }
        if carry == 0.into() {
            let mut entries = vec![];
            for (deg, coeff) in bottom_line.into_iter().enumerate() {
                if coeff != 0.into() {
                    entries.push(Term::new(coeff, vec![(var.clone(), deg - 1)]))
                }
            }
            Ok(Self::new(entries))
        } else {
            Err(())
        }
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
            Function::Product(Box::new(Function::Constant(self.0)), Box::new(res))
                .eval(&HashMap::new())
        } else {
            Function::Constant(self.0)
        }
    }
}

impl<TEntry: Ring> Into<Function<TEntry>> for Polynomial<TEntry> {
    fn into(self) -> Function<TEntry> {
        let mut entry_iter = self.entries.iter();
        if let Some(first) = entry_iter.next() {
            let mut f = first.clone().into();
            for next in entry_iter {
                f = Function::Sum(Box::new(next.clone().into()), Box::new(f));
            }
            f
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
            new_entries.push(Term::new(e.0.negate(), e.1));
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
                    re.1.clone().into_iter().chain(le.1.into_iter()).collect(),
                ))
            }
        }
        Self::new(new_entries)
    }
}

impl<TEntry: Ring> Ring for Polynomial<TEntry> {
    fn try_inverse(&self) -> Option<Self> {
        None
    }

    fn negate(&self) -> Self {
        Self::new(
            self.entries
                .iter()
                .map(|e| Term::new(e.0.negate(), e.1.clone()))
                .collect(),
        )
    }

    fn additive_ident() -> Self {
        Self::new(vec![])
    }

    fn multiplicative_ident() -> Self {
        Self::new(vec![Term::new(TEntry::multiplicative_ident(), vec![])])
    }

    fn generate(rng: &mut rand::prelude::ThreadRng, basic: bool) -> Self {
        let mut terms = vec![];
        for _ in 0..rng.random_range(if basic { 1..3 } else { 2..10 }) {
            let mut vars = vec![];
            for _ in 0..rng.random_range(if basic { 1..2 } else { 1..4 }) {
                let v = rng.random_range(0..VARS.len());
                vars.push((
                    VARS[v..v].to_string(),
                    rng.random_range(if basic { 1..2 } else { 1..5 }),
                ));
            }
            terms.push(Term::new(TEntry::generate(rng, basic), vars));
        }
        Self::new(terms)
    }
}

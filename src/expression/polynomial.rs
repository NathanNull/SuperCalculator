use rand::Rng;
use reikna::factor::quick_factorize;
use std::{
    array,
    collections::{HashMap, HashSet},
    ops::{Add, Mul, Sub},
    usize,
};

use crate::{
    matrix::ColumnVector,
    num::rational::{gcf, Rational},
    r,
    ring_field::{Field, Ring},
    vector_space::Vector,
};

use super::function::Function;

fn format_term<TEntry: Field>(coeff: TEntry, pow: usize) -> String {
    let mut res = "".to_string();
    if coeff == TEntry::multiplicative_ident() && pow != 0 {
        // do nothing
    } else if coeff == TEntry::multiplicative_ident().negate() && pow != 0 {
        res += "-";
    } else {
        res += &format!("{:?}", coeff);
    }

    if pow != 0 {
        res += &format!("x");
        if pow != 1 {
            res += &format!("^{}", pow);
        }
    }

    res
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Polynomial<TEntry: Field, const DEGREE: usize> {
    entries: [TEntry; DEGREE],
}

impl<TEntry: Field, const DEGREE: usize> std::fmt::Debug for Polynomial<TEntry, DEGREE> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let terms = self
            .entries
            .iter()
            .enumerate()
            .rev()
            .filter(|(_,t)|t != &&TEntry::additive_ident())
            .map(|(i, t)| format_term(t.clone(), i))
            .collect::<Vec<_>>();
        for (idx, term) in terms.iter().enumerate() {
            if idx != 0 {
                write!(f, " ")?;
                if !term.starts_with('-') {
                    write!(f, "+")?;
                }
            }
            write!(f, "{}", term)?;
        }
        Ok(())
    }
}

impl<TEntry: Field, const DEGREE: usize> Polynomial<TEntry, DEGREE> {
    pub fn new(prec_entries: Vec<(TEntry, usize)>) -> Self {
        let mut entries = array::from_fn(|_| TEntry::additive_ident());
        for entry in prec_entries {
            entries[entry.1] = entries[entry.1].clone() + entry.0;
        }
        Self { entries }
    }

    fn lowest_degree(&self) -> usize {
        for (i, coeff) in self.entries.iter().enumerate() {
            if coeff != &TEntry::additive_ident() {
                return i;
            }
        }
        return 0;
    }

    fn highest_degree(&self) -> usize {
        for (i, coeff) in self.entries.iter().enumerate().rev() {
            if coeff != &TEntry::additive_ident() {
                return i;
            }
        }
        return 0;
    }
}

// impl<TEntry: Field> Term<TEntry> {
//     pub fn new(coeff: TEntry, vars_iter: Vec<(String, usize)>) -> Self {
//         let mut vars: Vec<(String, usize)> = vec![];
//         for v in vars_iter {
//             if let Some(same) = vars.iter_mut().find(|sv| sv.0 == v.0) {
//                 same.1 += v.1;
//             } else if v.1 != 0 {
//                 vars.push(v);
//             }
//         }
//         Self(coeff, vars)
//     }

//     pub fn like_term(&self, other: &Self) -> bool {
//         // Make sure other contains everything self does
//         for (var, pow) in self.1.iter() {
//             if !other.1.iter().any(|(v, p)| v == var && p == pow) {
//                 return false;
//             }
//         }
//         // As long as it does (and contains no more elements), it's valid
//         self.1.len() == other.1.len()
//     }

//     fn var_to_fn(var: String, pow: usize) -> Function<TEntry> {
//         let mut res = Function::Constant(TEntry::multiplicative_ident());
//         for _ in 0..pow {
//             res = Function::Product(Box::new(Function::Variable(var.clone())), Box::new(res));
//         }
//         res.eval(&HashMap::new())
//     }

//     fn degree(&self) -> usize {
//         self.1.iter().fold(0, |acc, (_, p)| acc + *p)
//     }
// }

impl<const DEGREE: usize> Polynomial<Rational, DEGREE> {
    pub fn zeros(&self) -> Result<Vec<Rational>, ()> {
        // Using the rational root theorem, i.e. for a polynomial in one variable with
        // integer coefficients, any rational zeros are of the form p/q, with p being a factor
        // of the constant term and q being a factor of the highest degree term.

        if self.lowest_degree() != 0 {
            // All terms have at least one variable, so we need to divide it out before anything else.
            let new_poly = self.synthetic_divide(r!(0))?;
            let mut ret = new_poly.zeros()?;
            ret.push(r!(0));
            return Ok(ret);
        }

        let mut gcd = 1;
        for (_, c) in self.entries.iter().enumerate() {
            gcd = gcf(gcd, c.den());
        }
        let gcd_r = Rational::new(true, gcd, 1);
        let a0 = (self.entries[0] * gcd_r).num();
        let an = (self.entries[self.highest_degree()] * gcd_r).num();
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
                    map.insert("x".to_string(), Function::Constant(v));
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
                    if tmp_f == Self::new(vec![(r!(1), 0)]) {
                        break 'choose;
                    }
                }
            }
        }
        Ok(zeros)
    }

    fn synthetic_divide(&self, val: Rational) -> Result<Self, ()> {
        let max_deg = self.highest_degree();
        let mut carry = 0.into();
        let mut bottom_line = vec![r!(0); max_deg + 1];
        for d in (0..=max_deg).rev() {
            let top = self.entries[d];
            carry = top + carry * val;
            bottom_line[d] = carry;
        }
        if carry == 0.into() {
            let mut entries = vec![];
            for (deg, coeff) in bottom_line.into_iter().enumerate() {
                if coeff != 0.into() {
                    entries.push((coeff, deg - 1))
                }
            }
            Ok(Self::new(entries))
        } else {
            Err(())
        }
    }
}

impl<TEntry: Field> Into<Function<TEntry>> for (usize, TEntry) {
    fn into(self) -> Function<TEntry> {
        let mut res = Function::Constant(self.1);
        for _ in 0..self.0 {
            res = Function::Product(Box::new(res), Box::new(Function::Variable("x".to_string())));
        }
        res
    }
}

impl<TEntry: Field, const DEGREE: usize> Into<Function<TEntry>> for Polynomial<TEntry, DEGREE> {
    fn into(self) -> Function<TEntry> {
        let mut entry_iter = self.entries.into_iter().enumerate();
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

impl<TEntry: Field, const DEGREE: usize> Add for Polynomial<TEntry, DEGREE> {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        for (idx, e) in rhs.entries.into_iter().enumerate() {
            self.entries[idx] = self.entries[idx].clone() + e;
        }
        self
    }
}

impl<TEntry: Field, const DEGREE: usize> Sub for Polynomial<TEntry, DEGREE> {
    type Output = Self;

    fn sub(mut self, rhs: Self) -> Self::Output {
        for (idx, e) in rhs.entries.into_iter().enumerate() {
            self.entries[idx] = self.entries[idx].clone() - e;
        }
        self
    }
}

impl<TEntry: Field, const DEGREE: usize> Mul for Polynomial<TEntry, DEGREE> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut new_entries = array::from_fn(|_| TEntry::additive_ident());
        for (i, re) in rhs.entries.into_iter().enumerate() {
            for (j, le) in self.entries.iter().enumerate() {
                if i + j < DEGREE {
                    // FIXME: we're just dropping terms with degree greater than the max, is this okay?
                    // I feel like this isn't okay.
                    new_entries[i + j] = new_entries[i + j].clone() + (re.clone() * le.clone());
                }
            }
        }
        Self {
            entries: new_entries,
        }
    }
}

impl<TEntry: Field, const DEGREE: usize> From<TEntry> for Polynomial<TEntry, DEGREE> {
    fn from(value: TEntry) -> Self {
        Into::<Function<TEntry>>::into(value).try_into().unwrap()
    }
}

impl<TEntry: Field, const DEGREE: usize> Ring for Polynomial<TEntry, DEGREE> {
    fn try_inverse(&self) -> Option<Self> {
        None
    }

    fn negate(&self) -> Self {
        Self::new(
            self.entries
                .iter()
                .enumerate()
                .map(|e| (e.1.negate(), e.0))
                .collect(),
        )
    }

    fn additive_ident() -> Self {
        Self::new(vec![])
    }

    fn multiplicative_ident() -> Self {
        Self::new(vec![(TEntry::multiplicative_ident(), 0)])
    }

    fn generate(rng: &mut rand::prelude::ThreadRng, basic: bool) -> Self {
        let mut terms = vec![];
        for _ in 0..rng.random_range(if basic { 1..3 } else { 2..10 }) {
            terms.push((
                TEntry::generate(rng, basic),
                rng.random_range(if basic { 0..=2 } else { 0..=10 }),
            ));
        }
        Self::new(terms)
    }
}

impl<TEntry: Field, const DEGREE: usize> Mul<TEntry> for Polynomial<TEntry, DEGREE> {
    type Output = Self;

    fn mul(self, rhs: TEntry) -> Self::Output {
        let mut entries = vec![];
        for e in self.entries.into_iter().enumerate() {
            entries.push((e.1 * rhs.clone(), e.0));
        }
        Self::new(entries)
    }
}

impl<TEntry: Field, const DEGREE: usize> Vector<TEntry, DEGREE> for Polynomial<TEntry, DEGREE> {
    fn to_column(&self) -> ColumnVector<TEntry, DEGREE> {
        ColumnVector::v_new(self.entries.clone())
    }

    fn zero() -> Self {
        Self::new(vec![])
    }

    fn from_column(column: &ColumnVector<TEntry, DEGREE>) -> Self {
        Self {
            entries: column.entries.each_ref().map(|r|r[0].clone()),
        }
    }
}

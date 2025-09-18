use itertools::{EitherOrBoth, Itertools};
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
    num::{
        rational::{gcf, Rational},
        real::Real,
    },
    r,
    ring_field::{Field, FromUsize, Ring},
    vector_space::Vector,
};

use super::function::Function;

fn format_term<TEntry: Ring>(coeff: TEntry, pow: usize) -> String {
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
pub struct Polynomial<TEntry: Ring, const DEGREE: usize> {
    entries: [TEntry; DEGREE],
}

impl<TEntry: Ring, const DEGREE: usize> std::fmt::Debug for Polynomial<TEntry, DEGREE> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.into_unsized())
    }
}

impl<TEntry: Ring> std::fmt::Debug for UnsizedPolynomial<TEntry> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        let terms = self
            .entries
            .iter()
            .enumerate()
            .rev()
            .filter(|(_, t)| t != &&TEntry::additive_ident())
            .map(|(i, t)| format_term(t.clone(), i))
            .collect::<Vec<_>>();
        let mut wrote = false;
        for (idx, term) in terms.iter().enumerate() {
            if idx != 0 {
                write!(f, " ")?;
                if !term.starts_with('-') {
                    write!(f, "+")?;
                }
            }
            write!(f, "{}", term)?;
            wrote = true;
        }
        if !wrote {
            write!(f, "0")?
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl<TEntry: Ring, const DEGREE: usize> Polynomial<TEntry, DEGREE> {
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

    pub fn highest_degree(&self) -> usize {
        for (i, coeff) in self.entries.iter().enumerate().rev() {
            if coeff != &TEntry::additive_ident() {
                return i;
            }
        }
        return 0;
    }

    pub fn terms(&self) -> [TEntry; DEGREE] {
        self.entries.clone()
    }

    pub fn change_degree<const NEW: usize>(&self) -> Polynomial<TEntry, NEW> {
        Polynomial {
            entries: array::from_fn(|i| {
                if i < DEGREE {
                    self.entries[i].clone()
                } else {
                    TEntry::additive_ident()
                }
            }),
        }
    }

    pub fn into_unsized(&self) -> UnsizedPolynomial<TEntry> {
        let max_deg = self.highest_degree();
        let mut terms = vec![TEntry::additive_ident(); max_deg];
        for (pow, term) in self.entries.iter().enumerate() {
            terms[pow] = term.clone();
        }
        UnsizedPolynomial { entries: terms }
    }
}

pub trait PolynomialSolvable: Field + FromUsize {
    fn zeros(polynomial: &UnsizedPolynomial<Self>) -> Result<Vec<Self>, ()>;
    fn zeros_sized<const DEGREE: usize>(
        polynomial: &Polynomial<Self, DEGREE>,
    ) -> Result<Vec<Self>, ()> {
        Self::zeros(&polynomial.into_unsized())
    }
}

impl UnsizedPolynomial<Rational> {
    fn try_factor_constant(mut self) -> Self {
        let mut entry_iter = self.entries().iter().rev();
        if let Some(mut tmp_gcf) = entry_iter.next().cloned() {
            for next in entry_iter {
                if *next != r!(0) {
                    tmp_gcf = Rational::new(
                        true,
                        gcf(tmp_gcf.num(), next.num()),
                        gcf(tmp_gcf.den(), next.den()),
                    )
                }
            }
            self.entries = self.entries.into_iter().map(|e| e / tmp_gcf.clone()).collect();
        }
        self
    }
}

impl PolynomialSolvable for Rational {
    fn zeros(poly: &UnsizedPolynomial<Self>) -> Result<Vec<Self>, ()> {
        // Using the rational root theorem, i.e. for a polynomial in one variable with
        // integer coefficients, any rational zeros are of the form p/q, with p being a factor
        // of the constant term and q being a factor of the highest degree term.

        if poly.entries[0] == r!(0) {
            // All terms have at least one variable, so we need to divide it out before anything else.
            let new_poly = poly.synthetic_divide(r!(0))?;
            let mut ret = new_poly.zeros()?;
            ret.push(r!(0));
            return Ok(ret);
        }

        let mut gcd = 1;
        for (_, c) in poly.entries.iter().enumerate() {
            gcd = gcf(gcd, c.den());
        }
        let gcd_r = Rational::new(true, gcd, 1);
        let a0 = (poly.entries[0] * gcd_r).num();
        let an = (poly.entries[poly.entries.len() - 1] * gcd_r).num();
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
        let mut tmp_f = poly.clone().try_factor_constant();
        'choose: for p in &a0_f {
            for q in &an_f {
                for sign in [true, false] {
                    let v = Rational::new(sign, *p, *q);
                    map.insert("x".to_string(), Function::Constant(v));
                    // if f(p/q) = 0
                    while Into::<Function<Rational>>::into(tmp_f.clone()).eval(&map)
                        == Function::Constant(Rational::additive_ident())
                    {
                        zeros.push(v);
                        tmp_f = tmp_f.synthetic_divide(v)?.try_factor_constant();
                    }
                    // We've divided out everything and are just left with 1
                    if tmp_f == UnsizedPolynomial::new(vec![(r!(1), 0)]) {
                        break 'choose;
                    }
                }
            }
        }
        Ok(zeros)
    }
}

const MAX_ITERATIONS: usize = 100;
const EPSILON: f64 = 1e-7;
impl PolynomialSolvable for Real {
    fn zeros(polynomial: &UnsizedPolynomial<Self>) -> Result<Vec<Self>, ()> {
        let mut guess = Real(1.);
        let mut var_map = HashMap::from_iter([("x".to_string(), Function::Constant(guess))]);
        for _ in 0..MAX_ITERATIONS {
            if Into::<Function<_>>::into(polynomial.clone())
                .eval(&var_map)
                .as_constant()
                .0
                .abs()
                < EPSILON
            {
                return Ok(vec![guess]);
            }
            guess = polynomial.newton_iterate(guess);
            var_map.insert("x".to_string(), Function::Constant(guess));
        }
        Err(())
    }
}

impl<TEntry: PolynomialSolvable, const DEGREE: usize> Polynomial<TEntry, DEGREE> {
    pub fn zeros_sized(&self) -> Result<Vec<TEntry>, ()> {
        TEntry::zeros_sized(self)
    }
}

impl<TEntry: PolynomialSolvable> UnsizedPolynomial<TEntry> {
    pub fn zeros(&self) -> Result<Vec<TEntry>, ()> {
        TEntry::zeros(self)
    }

    pub fn synthetic_divide(&self, val: TEntry) -> Result<Self, ()> {
        let max_deg = self.entries.len() - 1;
        let mut carry = TEntry::additive_ident();
        let mut bottom_line = vec![TEntry::additive_ident(); max_deg + 1];
        for d in (0..=max_deg).rev() {
            let top = self.entries[d].clone();
            carry = top + carry * val.clone();
            bottom_line[d] = carry.clone();
        }
        if carry == TEntry::additive_ident() {
            let mut entries = vec![];
            for (deg, coeff) in bottom_line.into_iter().enumerate() {
                if coeff != TEntry::additive_ident() {
                    entries.push((coeff, deg - 1))
                }
            }
            Ok(Self::new(entries))
        } else {
            Err(())
        }
    }

    fn deriv(&self) -> Self {
        let mut res = Self::new(vec![]);
        for i in 0..self.entries.len() - 2 {
            res.entries
                .push(self.entries[i + 1].clone() * TEntry::from(i));
        }
        res
    }

    pub fn newton_iterate(&self, val: TEntry) -> TEntry {
        let deriv = self.deriv();
        let var_map = &HashMap::from_iter([("x".to_string(), Function::Constant(val.clone()))]);
        let deriv_val = Into::<Function<TEntry>>::into(deriv)
            .eval(var_map)
            .as_constant();
        let self_val = Into::<Function<TEntry>>::into(self.clone())
            .eval(var_map)
            .as_constant();
        if deriv_val == TEntry::additive_ident() {
            val
        } else {
            val - (self_val / deriv_val)
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
                } else if *le != TEntry::additive_ident() && re != TEntry::additive_ident() {
                    panic!("fail lmao @\n{i}\n{le:?}\n{j}\n{re:?}");
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

impl<TEntry: Field> Mul<TEntry> for UnsizedPolynomial<TEntry> {
    type Output = Self;

    fn mul(self, rhs: TEntry) -> Self::Output {
        let mut entries = vec![];
        for e in self.entries.into_iter().enumerate() {
            entries.push((e.1 * rhs.clone(), e.0));
        }
        Self::new(entries)
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
            entries: column.entries.each_ref().map(|r| r[0].clone()),
        }
    }
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct UnsizedPolynomial<TEntry: Ring> {
    entries: Vec<TEntry>,
}

impl<TEntry: Ring> UnsizedPolynomial<TEntry> {
    pub fn convert<TNew: Ring>(
        &self,
        converter: impl Fn(TEntry) -> TNew,
    ) -> UnsizedPolynomial<TNew> {
        UnsizedPolynomial {
            entries: self
                .entries
                .iter()
                .map(|e| (converter)(e.clone()))
                .collect(),
        }
    }

    pub fn new(prec_entries: Vec<(TEntry, usize)>) -> Self {
        let mut entries = vec![
            TEntry::additive_ident();
            prec_entries.iter().map(|(_, p)| *p).max().unwrap_or(0) + 1
        ];
        for entry in prec_entries {
            entries[entry.1] = entries[entry.1].clone() + entry.0;
        }
        Self { entries }
    }

    pub fn entries(&self) -> &Vec<TEntry> {
        &self.entries
    }
}

impl<TEntry: Ring> Add for UnsizedPolynomial<TEntry> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::new(
            self.entries
                .into_iter()
                .zip_longest(rhs.entries)
                .enumerate()
                .map(|(pow, aorb)| {
                    (
                        match aorb {
                            EitherOrBoth::Both(a, b) => a + b,
                            EitherOrBoth::Left(a) | EitherOrBoth::Right(a) => a,
                        },
                        pow,
                    )
                })
                .collect(),
        )
    }
}

impl<TEntry: Ring> Sub for UnsizedPolynomial<TEntry> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            entries: self
                .entries
                .into_iter()
                .zip_longest(rhs.entries)
                .map(|aorb| match aorb {
                    EitherOrBoth::Both(a, b) => a - b,
                    EitherOrBoth::Left(a) => a,
                    EitherOrBoth::Right(b) => b.negate(),
                })
                .collect(),
        }
    }
}

impl<TEntry: Ring> Mul for UnsizedPolynomial<TEntry> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut res = vec![];
        for (i, lv) in self.entries.into_iter().enumerate() {
            for (j, rv) in rhs.entries.iter().enumerate() {
                let pow = i + j;
                if res.len() <= pow {
                    res.push(TEntry::additive_ident());
                }
                res[pow] = res[pow].clone() + (lv.clone() * rv.clone())
            }
        }
        Self { entries: res }
    }
}

impl<TEntry: Ring> Ring for UnsizedPolynomial<TEntry> {
    fn try_inverse(&self) -> Option<Self> {
        None
    }

    fn negate(&self) -> Self {
        Self {
            entries: self.entries.iter().map(|e| e.negate()).collect(),
        }
    }

    fn additive_ident() -> Self {
        Self { entries: vec![] }
    }

    fn multiplicative_ident() -> Self {
        Self {
            entries: vec![TEntry::multiplicative_ident()],
        }
    }

    fn generate(_rng: &mut rand::prelude::ThreadRng, _basic: bool) -> Self {
        todo!("generation for unsized polynomials not implemented")
    }
}

impl<TEntry: Field> Into<Function<TEntry>> for UnsizedPolynomial<TEntry> {
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

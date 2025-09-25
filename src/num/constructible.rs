use std::{
    collections::HashMap,
    ops::{Add, Div, Mul, Neg, Sub},
};

use super::{rational::Rational, real::Real};
use crate::{
    expression::{function::Function, polynomial::UnsizedPolynomial},
    matrix::UnsizedMatrix,
    r,
    ring_field::{Ring, Sqrt, TrueDiv},
};

// TODO: custom Debug implementation which reforms a legal expression which produces the number that a given polynomial stores.
#[derive(Hash, Debug, PartialEq, Eq, Clone)]
pub struct Constructible {
    pub polynomial: UnsizedPolynomial<Rational>,
    approx: Real,
}

impl Constructible {
    pub fn from_poly(poly: UnsizedPolynomial<Rational>, approx: Real) -> Self {
        Self {
            polynomial: poly,
            approx,
        }
    }

    pub fn test_approx(&self, approx: Real) -> Real {
        let sum_fn: Function<Real> = self.polynomial.convert(|r| Real(r.as_f64())).into();
        sum_fn
            .eval(&HashMap::from_iter([(
                "x".to_string(),
                Function::Constant(approx),
            )]))
            .as_constant()
    }

    pub fn approx(&self) -> Real {
        self.approx
    }
}

impl From<Rational> for Constructible {
    fn from(value: Rational) -> Self {
        let num = Rational::new(value.positive(), value.num(), 1);
        let den = Rational::new(true, value.den(), 1);
        Self {
            polynomial: UnsizedPolynomial::new(vec![(den, 1), (num, 0)]),
            approx: Real(value.as_f64()),
        }
    }
}

fn choose(n: usize, k: usize) -> usize {
    // n choose k = n!/k!(n-k)!
    // = [n*(n-1)*(n-2)*...*(n-k+1)] / k!
    ((n - k + 1)..=n).product::<usize>() / (1..=k).product::<usize>()
}

// Resultant, the determinant of the Sylvester matrix
fn resultant(
    f: UnsizedPolynomial<UnsizedPolynomial<Rational>>,
    g: UnsizedPolynomial<UnsizedPolynomial<Rational>>,
    approx: Real,
) -> (UnsizedPolynomial<Rational>, Real) {
    let deg_f = f.entries().len() - 1;
    let deg_g = g.entries().len() - 1;

    let mut rows = vec![];
    let zero = UnsizedPolynomial::additive_ident();

    let f_coeffs = (0..=deg_f)
        .map(|i| &f.entries()[deg_f - i])
        .collect::<Vec<_>>();
    for i in 0..deg_g {
        let mut row = vec![];
        for j in 0..deg_f + deg_g {
            row.push(if j < i || j > i + deg_f {
                &zero
            } else {
                f_coeffs[j - i]
            })
        }
        rows.push(row)
    }

    let g_coeffs = (0..=deg_g)
        .map(|i| &g.entries()[deg_g - i])
        .collect::<Vec<_>>();
    for i in 0..deg_f {
        let mut row = vec![];
        for j in 0..deg_f + deg_g {
            row.push(if j < i || j > i + deg_g {
                &zero
            } else {
                g_coeffs[j - i]
            })
        }
        rows.push(row)
    }

    let sylvester_matrix = UnsizedMatrix::new(rows);
    let mut det = sylvester_matrix.determinant();
    let mut new_approx = approx;
    let f_det = det.convert(|r| Real(r.as_f64()));
    for _ in 0..10 {
        new_approx = f_det.newton_iterate(new_approx);
    }

    const EPSILON: f64 = 1e-3;
    if let Ok(zeros) = det.zeros() {
        let mut kept_factor = false;
        for zero in zeros {
            if (zero.as_f64() - new_approx.0) >= EPSILON || kept_factor {
                det = det
                    .synthetic_divide(zero)
                    .expect("We were told this was a zero")
            }
            if (zero.as_f64() - new_approx.0) < EPSILON {
                kept_factor = true;
            }
        }
    }
    if !det.entries().last().unwrap_or(&r!(0)).positive() {
        det = det * r!(-1)
    }
    (det, new_approx)
}

impl Add for Constructible {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        let deg_f = self.polynomial.entries().len() - 1;
        // Sum m=0 to deg(P) (-1^m) * [sum n=0 to deg(P)-m a_(n+m) (n+m choose m) z^n] * y^m
        let f = UnsizedPolynomial::new(
            (0..=deg_f)
                .map(|m| {
                    let n1_pow = if m % 2 == 0 { r!(1) } else { r!(-1) };
                    (
                        UnsizedPolynomial::new(
                            (0..=deg_f - m)
                                .map(|n| {
                                    let npm = n + m;
                                    (
                                        n1_pow
                                            * self.polynomial.entries()[npm]
                                            * Rational::new(true, choose(npm, m) as u64, 1),
                                        n,
                                    )
                                })
                                .collect(),
                        ),
                        m,
                    )
                })
                .collect(),
        );
        let g = rhs
            .polynomial
            .convert(|r| UnsizedPolynomial::new(vec![(r, 0)]));
        let approx = self.approx + rhs.approx;
        let (r, approx) = resultant(f, g, approx);
        Self {
            polynomial: r,
            approx,
        }
    }
}

impl Neg for Constructible {
    type Output = Self;

    fn neg(self) -> Self::Output {
        let negated_polynomial: UnsizedPolynomial<Rational> = UnsizedPolynomial::new(
            self.polynomial
                .entries()
                .iter()
                .enumerate()
                .map(|(pow, coeff)| {
                    (
                        if pow % 2 == 0 {
                            coeff.clone()
                        } else {
                            -coeff.clone()
                        },
                        pow,
                    )
                })
                .collect(),
        );
        Self {
            approx: -self.approx,
            polynomial: negated_polynomial,
        }
    }
}

impl Sub for Constructible {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        self + -rhs
    }
}

impl Mul for Constructible {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        // z = x * y => x = z / y => ax^2 + bx + c = f(x) = f(z/y) = 0 => az^2 + byz = cy^2 = 0
        let deg_f = self.polynomial.entries().len() - 1;
        let mut zeros = [r!(0)].into_iter().cycle();
        let f = UnsizedPolynomial::new(
            (0..=deg_f)
                .map(|n| {
                    (
                        UnsizedPolynomial::new(
                            (&mut zeros)
                                .take(deg_f - n)
                                .chain([self.polynomial.entries()[deg_f - n]])
                                .enumerate()
                                .map(|(a, b)| (b, a))
                                .collect(),
                        ),
                        n,
                    )
                })
                .collect(),
        );
        let g = rhs
            .polynomial
            .convert(|r| UnsizedPolynomial::new(vec![(r, 0)]));
        let approx = self.approx * rhs.approx;
        let (r, approx) = resultant(f, g, approx);
        Self {
            polynomial: r,
            approx,
        }
    }
}

impl Div for Constructible {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        // z = x / y => x = z * y => ax^2 + bx + c = f(x) = f(z/y) = 0 => az^2y^2 + byz + c = 0
        let deg_f = self.polynomial.entries().len() - 1;
        let f = UnsizedPolynomial::new(
            (0..=deg_f)
                .map(|n| {
                    (
                        UnsizedPolynomial::new(vec![(self.polynomial.entries()[n], n)]),
                        n,
                    )
                })
                .collect(),
        );
        let g = rhs
            .polynomial
            .convert(|r| UnsizedPolynomial::new(vec![(r, 0)]));
        let approx = self.approx / rhs.approx;
        let (r, approx) = resultant(f, g, approx);
        Self {
            polynomial: r,
            approx,
        }
    }
}

impl TrueDiv for Constructible {
    fn inverse(&self) -> Self {
        Self::multiplicative_ident() / self.clone()
    }
}

impl Ring for Constructible {
    fn try_inverse(&self) -> Option<Self> {
        Some(self.inverse())
    }

    fn negate(&self) -> Self {
        -self.clone()
    }

    fn additive_ident() -> Self {
        Self::from_poly(UnsizedPolynomial::new(vec![(r!(1), 1)]), Real(0.0))
    }

    fn multiplicative_ident() -> Self {
        Self::from_poly(
            UnsizedPolynomial::new(vec![(r!(1), 1), (r!(-1), 0)]),
            Real(1.0),
        )
    }

    fn generate(rng: &mut rand::prelude::ThreadRng, basic: bool) -> Self {
        todo!("Constructible generation not implemented yet")
    }
}

impl Sqrt for Constructible {
    fn sqrt(&self) -> Self {
        let rooted: UnsizedPolynomial<Rational> = UnsizedPolynomial::new(
            self.polynomial
                .entries()
                .iter()
                .enumerate()
                .map(|(pow, coeff)| (coeff.clone(), pow * 2))
                .collect(),
        );
        Self {
            approx: self.approx.sqrt(),
            polynomial: rooted,
        }
    }
}

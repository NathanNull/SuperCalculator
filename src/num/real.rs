use std::{hash::Hash, ops::{Add, Div, Mul, Neg, Sub}};

use rand::{rngs::ThreadRng, Rng};

use crate::ring_field::{Ring, TrueDiv};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Real(pub f64);

impl Eq for Real {}

impl Hash for Real {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}

impl Add for Real {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self { 0: self.0 + rhs.0 }
    }
}

impl Sub for Real {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self { 0: self.0 - rhs.0 }
    }
}

impl Mul for Real {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self { 0: self.0 * rhs.0 }
    }
}

impl Div for Real {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self { 0: self.0 / rhs.0 }
    }
}

impl Neg for Real {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}

impl Ring for Real {
    fn try_inverse(&self) -> Option<Self> {
        Some(Self(1.) / *self)
    }

    fn negate(&self) -> Self {
        -*self
    }

    fn additive_ident() -> Self {
        Self(0.)
    }

    fn multiplicative_ident() -> Self {
        Self(1.)
    }

    fn generate(rng: &mut ThreadRng, basic: bool) -> Self {
        if basic {
            const OPTIONS: [f64; 4] = [1., 0., 2., -1.];
            Self(OPTIONS[rng.random_range(..OPTIONS.len())])
        } else {
            Self(rng.random::<f64>() % 1024.)
        }
    }

    fn from_usize(i: usize) -> Self {
        Self(i as f64)
    }
}

impl TrueDiv for Real {
    fn inverse(&self) -> Self {
        Self(1.) / *self
    }
}

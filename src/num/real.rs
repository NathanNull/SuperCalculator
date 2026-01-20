use std::{
    fmt::Debug,
    hash::Hash,
    ops::{Add, Div, Mul, Neg, Sub},
};

use rand::{Rng, rngs::ThreadRng};

use crate::{
    repl::{Value, ValueType},
    ring_field::{ExponentialRing, FieldOps, FromUsize, Ring, Sqrt, TrueDiv},
};

#[derive(Clone, Copy)]
pub struct Real(pub f64);

impl Debug for Real {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Real {
    pub fn sqrt(&self) -> Self {
        Real(self.0.sqrt())
    }
}

const EPSILON: f64 = 1e-7;
impl PartialEq for Real {
    fn eq(&self, other: &Self) -> bool {
        (self.0 - other.0).abs() < EPSILON
    }
}

impl Eq for Real {}

impl Hash for Real {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}

impl Add for Real {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Sub for Real {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl Mul for Real {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self(self.0 * rhs.0)
    }
}

impl Div for Real {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self(self.0 / rhs.0)
    }
}

impl Neg for Real {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}

impl Value for Real {
    fn get_type(&self) -> ValueType {
        ValueType::Real
    }

    fn try_op(
        &self,
        op: crate::repl::Op,
        rhs: Box<dyn Value>,
    ) -> Result<Box<dyn Value>, Box<dyn std::error::Error>> {
        self.try_field_ops(&*rhs, op)
            .ok_or_else(|| "Invalid real op".into())
    }
}

impl Ring for Real {
    fn try_inverse(&self) -> Option<Self> {
        Some(Self(1.) / *self)
    }

    fn negate(&self) -> Self {
        -*self
    }

    fn zero() -> Self {
        Self(0.)
    }

    fn one() -> Self {
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
}

impl TrueDiv for Real {
    fn inverse(&self) -> Self {
        Self(1.) / *self
    }
}

impl FromUsize for Real {
    fn from(value: usize) -> Self {
        Self(value as f64)
    }
}

#[allow(clippy::non_canonical_partial_ord_impl)]
impl PartialOrd for Real {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl Ord for Real {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.partial_cmp(&other.0).unwrap()
    }
}

impl Sqrt for Real {
    fn sqrt(&self) -> Self {
        self.sqrt()
    }
}

impl ExponentialRing for Real {
    fn exp(self) -> Self {
        Self(self.0.exp())
    }
}

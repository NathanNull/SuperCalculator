use std::{
    fmt::Debug,
    ops::{Add, Div, Mul, Sub},
};

use crate::{
    matrix::ColumnVector, num::real::Real, repl::{Value, ValueType}, ring_field::{ExponentialRing, Field, Ring, TrueDiv, try_field_ops}, vector_space::Vector
};

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct Complex {
    pub real: Real,
    pub imag: Real,
}

impl Complex {
    pub fn conjugate(&self) -> Self {
        Self {
            real: self.real,
            imag: -self.imag,
        }
    }
}

impl Debug for Complex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}{}{:?}i", self.real, if self.imag.0 < 0. {""} else {"+"}, self.imag)
    }
}

impl Mul for Complex {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            real: self.real * rhs.real - self.imag * rhs.imag,
            imag: self.real * rhs.imag + self.imag * rhs.real,
        }
    }
}

impl Mul<Real> for Complex {
    type Output = Self;

    fn mul(self, rhs: Real) -> Self::Output {
        Self {
            real: self.real * rhs,
            imag: self.imag * rhs,
        }
    }
}

impl Sub for Complex {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            real: self.real - rhs.real,
            imag: self.imag - rhs.imag,
        }
    }
}

impl Add for Complex {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            real: self.real + rhs.real,
            imag: self.imag + rhs.imag,
        }
    }
}

impl Ring for Complex {
    fn try_inverse(&self) -> Option<Self> {
        Some(self.inverse())
    }

    fn negate(&self) -> Self {
        Self {
            real: -self.real,
            imag: -self.imag,
        }
    }

    fn zero() -> Self {
        Self {
            real: Real(0.),
            imag: Real(0.),
        }
    }

    fn one() -> Self {
        Self {
            real: Real(1.),
            imag: Real(0.),
        }
    }

    fn generate(rng: &mut rand::prelude::ThreadRng, basic: bool) -> Self {
        Self {
            real: Real::generate(rng, basic),
            imag: Real::generate(rng, basic),
        }
    }
}

#[allow(clippy::suspicious_arithmetic_impl)]
impl Div for Complex {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        self * rhs.inverse()
    }
}

impl TrueDiv for Complex {
    fn inverse(&self) -> Self {
        let mag = self.square_magnitude();
        self.conjugate() * mag.inverse()
    }
}

impl Field for Complex {}

impl Vector<Real, 2> for Complex {
    fn to_column(&self) -> crate::matrix::ColumnVector<Real, 2> {
        ColumnVector::v_new([self.real, self.imag])
    }

    fn from_column(column: &crate::matrix::ColumnVector<Real, 2>) -> Self {
        Self {
            real: column.entries[0][0],
            imag: column.entries[1][0],
        }
    }

    fn vec_zero() -> Self {
        Ring::zero()
    }
}

impl ExponentialRing for Complex {
    fn exp(self) -> Self {
        let magnitude = self.real.exp();
        let real = Real(self.imag.0.cos()) * magnitude;
        let imag = Real(self.imag.0.sin()) * magnitude;
        Self { real, imag }
    }
}

impl Value for Complex {
    fn get_type(&self) -> ValueType {
        ValueType::Complex
    }

    fn try_op(&self, op: crate::repl::Op, rhs: Box<dyn Value>) -> Result<Box<dyn Value>, Box<dyn std::error::Error>> {
        try_field_ops(self, rhs.as_ref(), op).ok_or_else(|| "invalid op".into())
    }
}
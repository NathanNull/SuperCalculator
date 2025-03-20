use std::ops::{Add, Div, Mul, Sub};

use rand::{rngs::ThreadRng, Rng};

pub trait Ring:
    Add<Output = Self>
    + Sub<Output = Self>
    + Mul<Output = Self>
    + Copy
    + PartialEq
    + std::fmt::Debug
    + Sized
{
    fn try_inverse(&self) -> Option<Self>;
    fn negate(&self) -> Self;
    fn additive_ident() -> Self;
    fn multiplicative_ident() -> Self;
    fn generate(rng: &mut ThreadRng) -> Self;
}

impl Ring for i32 {
    fn try_inverse(&self) -> Option<Self> {
        if self == &1 || self == &-1 {
            Some(*self)
        } else {
            None
        }
    }

    fn negate(&self) -> Self {
        -self
    }

    fn additive_ident() -> Self {
        0
    }

    fn multiplicative_ident() -> Self {
        1
    }

    fn generate(rng: &mut ThreadRng) -> Self {
        rng.random()
    }
}

pub trait TrueDiv: Div<Output = Self> + Sized {
    fn inverse(&self) -> Self;
}

pub trait Field: Ring + TrueDiv {}
impl<T: Ring + TrueDiv> Field for T {}

impl Ring for f64 {
    fn try_inverse(&self) -> Option<Self> {
        Some(1. / self)
    }

    fn negate(&self) -> Self {
        -self
    }

    fn additive_ident() -> Self {
        0.
    }

    fn multiplicative_ident() -> Self {
        1.
    }

    fn generate(rng: &mut ThreadRng) -> Self {
        rng.random::<Self>() % 1024.
    }
}

impl TrueDiv for f64 {
    fn inverse(&self) -> Self {
        1. / self
    }
}

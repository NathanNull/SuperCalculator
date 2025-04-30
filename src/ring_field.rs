use std::{
    hash::Hash,
    ops::{Add, Div, Mul, Sub},
};

use rand::{rngs::ThreadRng, Rng};

pub trait Convenient: Clone + Eq + std::fmt::Debug + Sized + Send + Sync + Hash + 'static {}
impl<T: Clone + Eq + std::fmt::Debug + Sized + Send + Sync + Hash + 'static> Convenient for T {}

pub trait Ring: Add<Output = Self> + Sub<Output = Self> + Mul<Output = Self> + Convenient {
    fn try_inverse(&self) -> Option<Self>;
    fn negate(&self) -> Self;
    fn additive_ident() -> Self;
    fn multiplicative_ident() -> Self;
    fn generate(rng: &mut ThreadRng, basic: bool) -> Self;
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

    fn generate(rng: &mut ThreadRng, basic: bool) -> Self {
        if basic {
            rng.random_range(-4..4)
        } else {
            rng.random()
        }
    }
}

pub trait TrueDiv: Div<Output = Self> + Sized {
    fn inverse(&self) -> Self;
}

pub trait Field: Ring + TrueDiv {}
impl<T: Ring + TrueDiv> Field for T {}

pub trait FromUsize {
    fn from(val: usize) -> Self;
}

use std::{
    hash::Hash,
    ops::{Add, Div, Mul, Sub},
};

use rand::{rngs::ThreadRng, Rng};

use crate::repl::{Downcast, Op, Value, ValueType};

pub trait Convenient: Clone + Eq + std::fmt::Debug + Sized + Send + Sync + Hash + 'static {}
impl<T: Clone + Eq + std::fmt::Debug + Sized + Send + Sync + Hash + 'static> Convenient for T {}

pub trait Ring: Add<Output = Self> + Sub<Output = Self> + Mul<Output = Self> + Convenient {
    fn try_inverse(&self) -> Option<Self>;
    fn negate(&self) -> Self;
    fn zero() -> Self;
    fn one() -> Self;
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

    fn zero() -> Self {
        0
    }

    fn one() -> Self {
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

pub trait Sqrt {
    fn sqrt(&self) -> Self;
}

pub trait QuadraticClosure: Field + Sqrt {}
impl<T: Field + Sqrt> QuadraticClosure for T {}

impl Value for i32 {
    fn get_type(&self) -> ValueType {
        ValueType::Integer
    }

    fn try_op(
        &self,
        op: Op,
        rhs: Box<dyn Value>,
    ) -> Result<Box<dyn Value>, Box<dyn std::error::Error>> {
        try_ring_ops(self, &*rhs, op).ok_or_else(|| "Invalid int op".into())
    }
}

pub fn try_ring_ops<T: Ring + Value>(lhs: &T, rhs: &dyn Value, op: Op) -> Option<Box<dyn Value>> {
    if rhs.get_type() == lhs.get_type() {
        let rhs = rhs.downcast::<T>().expect("Downcast error");
        Some(Box::new(match op {
            Op::Add => lhs.clone() + rhs.clone(),
            Op::Sub => lhs.clone() - rhs.clone(),
            Op::Mul => lhs.clone() * rhs.clone(),
            _ => return None,
        }))
    } else {
        None
    }
}

pub fn try_field_ops<T: Field + Value>(lhs: &T, rhs: &dyn Value, op: Op) -> Option<Box<dyn Value>> {
    if let Some(res) = try_ring_ops(lhs, rhs, op) {
        return Some(res)
    }
    if rhs.get_type() == lhs.get_type() {
        let rhs = rhs.downcast::<T>().expect("Downcast error");
        Some(Box::new(match op {
            Op::Div => lhs.clone() / rhs.clone(),
            _ => return None,
        }))
    } else {
        None
    }
}

/// Requires that exp(a+b)=exp(a)*exp(b)
pub trait ExponentialRing: Ring {
    fn exp(self) -> Self;
}

impl ExponentialRing for i32 {
    fn exp(self) -> Self {
        (-1i32).pow(((self%2) + 2) as u32)
    }
}
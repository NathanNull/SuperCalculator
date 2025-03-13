use std::ops::{Add, Div, Mul, Sub};

pub trait Ring:
    Add<Output = Self> + Sub<Output = Self> + Mul<Output = Self> + Copy + Eq + std::fmt::Debug + Sized
{
    fn try_inverse(&self) -> Option<Self>;
    fn negate(&self) -> Self;
    fn additive_ident() -> Self;
    fn multiplicative_ident() -> Self;
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
}

pub trait TrueDiv: Div<Output=Self> + Sized {
    fn inverse(&self) -> Self;
}

pub trait Field: Ring + TrueDiv {}
impl<T: Ring + TrueDiv> Field for T {}
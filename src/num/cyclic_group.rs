use std::ops::{Add, Div, Mul, Sub};

use rand::Rng;

use crate::{
    if_trait::{If, True},
    ring_field::{Ring, TrueDiv},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ZMod<const N: usize> {
    val: usize,
}

impl<const N: usize> Into<usize> for ZMod<N> {
    fn into(self) -> usize {
        self.val
    }
}

impl<const N: usize> std::fmt::Debug for ZMod<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}

impl<const N: usize> ZMod<N> {
    pub fn new(val: usize) -> Self {
        Self { val: val % N }
    }
}

impl<const N: usize> Add for ZMod<N> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            val: (self.val + rhs.val) % N,
        }
    }
}

impl<const N: usize> Sub for ZMod<N> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            val: (N + self.val - rhs.val) % N,
        }
    }
}

impl<const N: usize> Mul for ZMod<N> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            val: (self.val * rhs.val) % N,
        }
    }
}

impl<const N: usize> Ring for ZMod<N> {
    fn try_inverse(&self) -> Option<Self> {
        let (g, x, _) = extended_gcd(self.val as isize, N as isize);
        if g != 1 {
            None // No inverse exists
        } else {
            Some(Self::new((x + N as isize) as usize)) // Ensure positive result
        }
    }

    fn negate(&self) -> Self {
        Self::new(N - self.val)
    }

    fn additive_ident() -> Self {
        Self::new(0)
    }

    fn multiplicative_ident() -> Self {
        Self::new(1)
    }

    fn generate(rng: &mut rand::prelude::ThreadRng, basic: bool) -> Self {
        Self::new(rng.random_range(..if basic { 2 % N } else { N }))
    }
}

impl<const N: usize> Div for ZMod<N>
where
    If<{ is_prime(N) }>: True,
{
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        self * rhs.inverse()
    }
}

impl<const N: usize> TrueDiv for ZMod<N>
where
    If<{ is_prime(N) }>: True,
{
    fn inverse(&self) -> Self {
        self.try_inverse().unwrap()
    }
}

const fn is_prime(n: usize) -> bool {
    if n < 2 {
        return false;
    }
    if n == 2 || n == 3 {
        return true;
    }
    if n % 2 == 0 || n % 3 == 0 {
        return false;
    }

    let mut i = 5;
    while i * i <= n {
        if n % i == 0 || n % (i + 2) == 0 {
            return false;
        }
        i += 6;
    }
    true
}

fn extended_gcd(a: isize, b: isize) -> (isize, isize, isize) {
    if a == 0 {
        return (b, 0, 1);
    } else {
        let (g, x1, y1) = extended_gcd(b % a, a);
        let x = y1 - (b / a) * x1;
        let y = x1;
        return (g, x, y);
    }
}

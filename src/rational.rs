use std::{
    mem::swap,
    ops::{Add, Div, Mul, Sub},
};

use crate::ring_field::{Ring, TrueDiv};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Rational {
    positive: bool,
    num: u64,
    den: u64,
}

impl std::fmt::Debug for Rational {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.positive {
            write!(f, "-")?;
        }
        write!(f, "{}", self.num)?;
        if self.den != 1 {
            write!(f, "/{}", self.den)?;
        }
        Ok(())
    }
}

impl Rational {
    pub fn new(positive: bool, num: u64, den: u64) -> Self {
        if num == 0 {
            return Self::additive_ident();
        }
        let factor = gcd(num, den);
        Self {
            positive,
            num: num / factor,
            den: den / factor,
        }
    }
}

pub fn gcd(mut u: u64, mut v: u64) -> u64 {
    // Base cases: gcd(n, 0) = gcd(0, n) = n
    if u == 0 {
        return v;
    } else if v == 0 {
        return u;
    }

    // Using identities 2 and 3:
    // gcd(2ⁱ u, 2ʲ v) = 2ᵏ gcd(u, v) with u, v odd and k = min(i, j)
    // 2ᵏ is the greatest power of two that divides both 2ⁱ u and 2ʲ v
    let i = u.trailing_zeros();
    u >>= i;
    let j = v.trailing_zeros();
    v >>= j;
    let k = i.min(j);

    loop {
        // u and v are odd at the start of the loop
        debug_assert!(u % 2 == 1, "u = {} should be odd", u);
        debug_assert!(v % 2 == 1, "v = {} should be odd", v);

        // Swap if necessary so u ≤ v
        if u > v {
            swap(&mut u, &mut v);
        }

        // Identity 4: gcd(u, v) = gcd(u, v-u) as u ≤ v and u, v are both odd
        v -= u;
        // v is now even

        if v == 0 {
            // Identity 1: gcd(u, 0) = u
            // The shift by k is necessary to add back the 2ᵏ factor that was removed before the loop
            return u << k;
        }

        // Identity 3: gcd(u, 2ʲ v) = gcd(u, v) as u is odd
        v >>= v.trailing_zeros();
    }
}

impl Mul for Rational {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::new(
            self.positive == rhs.positive,
            self.num * rhs.num,
            self.den * rhs.den,
        )
    }
}

impl Add for Rational {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if !rhs.positive {
            return self - rhs.negate();
        }
        let n1 = self.num * rhs.den;
        let n2 = rhs.num * self.den;
        let den = self.den * rhs.den;
        if self.positive {
            Self::new(true, n1 + n2, den)
        } else if n1 < n2 {
            Self::new(true, n2 - n1, den)
        } else {
            Self::new(false, n1 - n2, den)
        }
    }
}

impl Sub for Rational {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        if !rhs.positive {
            return self + rhs.negate();
        }
        let n1 = self.num * rhs.den;
        let n2 = rhs.num * self.den;
        let den = self.den * rhs.den;
        if !self.positive {
            Self::new(false, n1 + n2, den)
        } else if n1 < n2 {
            Self::new(false, n2 - n1, den)
        } else {
            Self::new(true, n1 - n2, den)
        }
    }
}

impl Div for Rational {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self::new(
            self.positive == rhs.positive,
            self.num * rhs.den,
            self.den * rhs.num,
        )
    }
}

impl TrueDiv for Rational {
    fn inverse(&self) -> Self {
        Self::new(self.positive, self.den, self.num)
    }
}

impl Ring for Rational {
    fn try_inverse(&self) -> Option<Self> {
        Some(self.inverse())
    }

    fn negate(&self) -> Self {
        Self {
            positive: !self.positive,
            ..*self
        }
    }

    fn additive_ident() -> Self {
        Self {
            positive: true,
            num: 0,
            den: 1,
        }
    }

    fn multiplicative_ident() -> Self {
        Self {
            positive: true,
            num: 1,
            den: 1,
        }
    }
}

impl From<i32> for Rational {
    fn from(value: i32) -> Self {
        Self::new(value >= 0, value.abs() as u64, 1)
    }
}

#[macro_export]
macro_rules! r {
    (-$num:literal / $den:literal) => {
        Rational::new(false, $num, $den)
    };
    ($num:literal / $den:literal) => {
        Rational::new(true, $num, $den)
    };
    (-$num:literal) => {
        Rational::new(false, $num, 1)
    };
    ($num:literal) => {
        Rational::new(true, $num, 1)
    };
}

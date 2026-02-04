use crate::{matrix::SizedMatrix, num::rational::Rational, r};

/// Void type, basically static, exists to contain generic consts
pub enum Calculus<const DEGREE: usize> {}

impl<const DEGREE: usize> Calculus<DEGREE>
where
    [(); DEGREE - 1]:,
    [(); DEGREE + 1]:,
{
    const DER_ENTRIES: [[Rational; DEGREE]; DEGREE - 1] = {
        let mut entries = [[r!(0); DEGREE]; DEGREE - 1];
        let mut i = 0;
        while i < DEGREE - 1 {
            // Apparently you can't use for loops properly in const functions
            entries[i][i + 1] = Rational::new(true, (i + 1) as u64, 1);
            i += 1;
        }
        entries
    };
    pub fn get_derivative() -> SizedMatrix<Rational, { DEGREE - 1 }, DEGREE> {
        SizedMatrix::new(Self::DER_ENTRIES)
    }

    const INT_ENTRIES: [[Rational; DEGREE]; DEGREE + 1] = {
        let mut entries = [[r!(0); DEGREE]; DEGREE + 1];
        let mut i = 1;
        while i < DEGREE + 1 {
            // Apparently you can't use for loops properly in const functions
            entries[i][i - 1] = Rational::new(true, 1, i as u64);
            i += 1;
        }
        entries
    };
    pub fn get_integral() -> SizedMatrix<Rational, { DEGREE + 1 }, DEGREE> {
        SizedMatrix::new(Self::INT_ENTRIES)
    }
}

use super::*;
use std::ops::{Add, Mul, Sub};

impl<const R: usize, const C: usize, TEntry: Ring> Add for SizedMatrix<TEntry, R, C> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::from_unsized(self.inner + rhs).expect("Should be the right size")
    }
}
impl<const R: usize, const C: usize, TEntry: Ring> Sub for SizedMatrix<TEntry, R, C> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::from_unsized(self.inner - rhs).expect("Should be the right size")
    }
}
impl<const R: usize, const M: usize, const C: usize, TEntry: Ring> Mul<SizedMatrix<TEntry, M, C>>
    for SizedMatrix<TEntry, R, M>
{
    type Output = SizedMatrix<TEntry, R, C>;

    fn mul(self, rhs: SizedMatrix<TEntry, M, C>) -> Self::Output {
        SizedMatrix::from_unsized(self.inner * rhs.to_unsized()).expect("Should be the right size")
    }
}

impl<const R: usize, const C: usize, TEntry: Ring> Mul<TEntry> for SizedMatrix<TEntry, R, C> {
    type Output = Self;
    fn mul(mut self, rhs: TEntry) -> Self::Output {
        self.scale(rhs);
        self
    }
}

impl<TEntry: Ring, Rhs: Matrix<TEntry>> Add<Rhs> for UnsizedMatrix<TEntry> {
    type Output = Self;

    fn add(self, rhs: Rhs) -> Self::Output {
        let left_size = self.size();
        let right_size = rhs.size();
        assert_eq!(left_size, right_size, "Can't add matrices");
        UnsizedMatrix::fill_size(
            |i, j| self.get_entry(i, j).unwrap().clone() + rhs.get_entry(i, j).unwrap().clone(),
            left_size,
        )
    }
}

impl<TEntry: Ring, Rhs: Matrix<TEntry>> Sub<Rhs> for UnsizedMatrix<TEntry> {
    type Output = Self;

    fn sub(self, rhs: Rhs) -> Self::Output {
        let left_size = self.size();
        let right_size = rhs.size();
        assert_eq!(left_size, right_size, "Can't sub matrices");
        UnsizedMatrix::fill_size(
            |i, j| self.get_entry(i, j).unwrap().clone() - rhs.get_entry(i, j).unwrap().clone(),
            left_size,
        )
    }
}

impl<TEntry: Ring> Mul<UnsizedMatrix<TEntry>> for UnsizedMatrix<TEntry> {
    type Output = Self;

    fn mul(self, rhs: UnsizedMatrix<TEntry>) -> Self::Output {
        let left_size = self.size();
        let right_size = rhs.size();
        assert_eq!(left_size.1, right_size.0, "Can't add matrices");
        UnsizedMatrix::fill_size(
            |i, j| {
                (0..left_size.1)
                    .map(|p| {
                        self.get_entry(i, p).unwrap().clone() * rhs.get_entry(p, j).unwrap().clone()
                    })
                    .reduce(|acc, v| acc + v)
                    .unwrap()
            },
            (left_size.0, right_size.1),
        )
    }
}

impl<TEntry: Ring> Mul<TEntry> for UnsizedMatrix<TEntry> {
    type Output = Self;
    fn mul(mut self, rhs: TEntry) -> Self::Output {
        self.scale(rhs);
        self
    }
}
use super::*;
use std::{
    array,
    ops::{Add, Mul, Sub},
};

impl<const R: usize, const C: usize, TEntry: Ring> Add for Matrix<TEntry, R, C> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            entries: array::from_fn(|i| {
                array::from_fn(|j| self.entries[i][j].clone() + rhs.entries[i][j].clone())
            }),
        }
    }
}
impl<const R: usize, const C: usize, TEntry: Ring> Sub for Matrix<TEntry, R, C> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            entries: array::from_fn(|i| {
                array::from_fn(|j| self.entries[i][j].clone() - rhs.entries[i][j].clone())
            }),
        }
    }
}
impl<const R: usize, const M: usize, const C: usize, TEntry: Ring> Mul<Matrix<TEntry, M, C>>
    for Matrix<TEntry, R, M>
{
    type Output = Matrix<TEntry, R, C>;

    fn mul(self, rhs: Matrix<TEntry, M, C>) -> Self::Output {
        Matrix {
            entries: array::from_fn(|r| {
                array::from_fn(|c| {
                    (0..M)
                        .map(|i| self.entries[r][i].clone() * rhs.entries[i][c].clone())
                        .reduce(|acc, v| acc + v)
                        .unwrap()
                })
            }),
        }
    }
}

use std::{
    array,
    ops::{Add, Mul},
};

use crate::{
    debug_multi::DebugMulti,
    if_trait::{If, True},
    matrix::{ColumnVector, SquareMatrix},
    ring_field::{Convenient, Field},
};

pub mod subspace;

pub trait Vector<TEntry: Field, const DIMENSION: usize>:
    Add<Output = Self> + Mul<TEntry, Output = Self> + Convenient + DebugMulti
{
    fn to_column(&self) -> ColumnVector<TEntry, DIMENSION>;
    fn from_column(column: &ColumnVector<TEntry, DIMENSION>) -> Self;
    fn zero() -> Self;
    fn dot(&self, other: &Self) -> TEntry {
        let col_self = self.to_column();
        let col_other = other.to_column();
        let mut res = TEntry::additive_ident();
        for ([s],[o]) in col_self.entries.into_iter().zip(col_other.entries) {
            res = res + (s * o);
        }
        res
    }
    fn triple_product(&self, v: &Self, w: &Self) -> TEntry where Self: Cross<TEntry> + Sized {
        self.dot(&v.cross(&w))
    }
    fn square_magnitude(&self) -> TEntry {
        self.dot(self)
    }
    fn project_onto(&self, other: &Self) -> Self {
        other.clone() * (self.dot(other)/other.dot(other))
    }
}

pub trait Cross<TEntry> {
    fn cross(&self, other: &Self) -> Self
    where
        Self: Sized;
}

impl<TEntry: Field, T: Vector<TEntry, 3>> Cross<TEntry> for T {
    fn cross(&self, other: &Self) -> Self
    where
        Self: Sized,
    {
        let [u1, u2, u3] = self.to_column().as_array();
        let [v1, v2, v3] = other.to_column().as_array();
        let res = [
            u2.clone() * v3.clone() - u3.clone() * v2.clone(),
            u3.clone() * v1.clone() - u1.clone() * v3.clone(),
            u1.clone() * v2.clone() - u2.clone() * v1.clone(),
        ];
        Self::from_column(&ColumnVector::v_new(res))
    }
}

impl<TEntry: Field, const N: usize> Vector<TEntry, N> for ColumnVector<TEntry, N> {
    fn to_column(&self) -> ColumnVector<TEntry, N> {
        self.clone()
    }
    fn zero() -> Self {
        Self::v_new(array::from_fn(|_| TEntry::additive_ident()))
    }

    fn from_column(column: &ColumnVector<TEntry, N>) -> Self {
        column.clone()
    }
}

impl<TEntry: Field, const N: usize> Vector<TEntry, { N * N }> for SquareMatrix<TEntry, N>
where
    If<{ N > 1 }>: True,
{
    fn to_column(&self) -> ColumnVector<TEntry, { N * N }> {
        let mut entries = array::from_fn(|_| TEntry::additive_ident());
        for r in 0..N {
            for c in 0..N {
                entries[r * N + c] = self.entries[r][c].clone();
            }
        }
        ColumnVector::v_new(entries)
    }
    fn zero() -> Self {
        Self::new(array::from_fn(|_| {
            array::from_fn(|_| TEntry::additive_ident())
        }))
    }

    fn from_column(column: &ColumnVector<TEntry, { N * N }>) -> Self {
        let mut entries = array::from_fn(|_| array::from_fn(|_| TEntry::additive_ident()));
        for r in 0..N {
            for c in 0..N {
                entries[r][c] = column.entries[r * N + c][0].clone();
            }
        }
        Self::new(entries)
    }
}

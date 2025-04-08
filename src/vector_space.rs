use std::{
    array,
    hash::Hash,
    ops::{Add, Mul},
};

use rand::rngs::ThreadRng;

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
    fn zero() -> Self;
}

impl<TEntry: Field, const N: usize> Vector<TEntry, N> for ColumnVector<TEntry, N> {
    fn to_column(&self) -> ColumnVector<TEntry, N> {
        self.clone()
    }
    fn zero() -> Self {
        Self::v_new(array::from_fn(|_| TEntry::additive_ident()))
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
}

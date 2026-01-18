use std::{
    array,
    ops::{Add, Mul, Sub},
};

use crate::{
    debug_multi::DebugMulti,
    if_trait::{If, True},
    matrix::{ColumnVector, Matrix},
    repl::{Downcast, Op, Value},
    ring_field::{Convenient, Field, QuadraticClosure, Ring},
};

pub mod subspace;

#[allow(unused)]
pub trait Vector<TEntry: Ring, const DIMENSION: usize>:
    Add<Output = Self> + Sub<Output = Self> + Mul<TEntry, Output = Self> + Convenient + DebugMulti
{
    fn to_column(&self) -> ColumnVector<TEntry, DIMENSION>;
    fn from_column(column: &ColumnVector<TEntry, DIMENSION>) -> Self;
    fn vec_zero() -> Self;
    fn dot(&self, other: &Self) -> TEntry {
        let col_self = self.to_column();
        let col_other = other.to_column();
        let mut res = TEntry::zero();
        for ([s], [o]) in col_self.entries.into_iter().zip(col_other.entries) {
            res = res + (s * o);
        }
        res
    }
    fn triple_product(&self, v: &Self, w: &Self) -> TEntry
    where
        Self: Cross<TEntry> + Sized,
    {
        self.dot(&v.cross(w))
    }
    fn square_magnitude(&self) -> TEntry {
        self.dot(self)
    }
    fn magnitude(&self) -> TEntry
    where
        TEntry: QuadraticClosure,
    {
        self.square_magnitude().sqrt()
    }
    fn project_onto(&self, other: &Self) -> Self
    where
        TEntry: Field,
    {
        other.clone() * (self.dot(other) / other.dot(other))
    }
}

pub trait Cross<TEntry> {
    fn cross(&self, other: &Self) -> Self
    where
        Self: Sized;
}

impl<TEntry: Ring, T: Vector<TEntry, 3>> Cross<TEntry> for T {
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

impl<TEntry: Ring, const N: usize> Vector<TEntry, N> for ColumnVector<TEntry, N> {
    fn to_column(&self) -> ColumnVector<TEntry, N> {
        self.clone()
    }
    fn vec_zero() -> Self {
        Self::v_new(array::from_fn(|_| TEntry::zero()))
    }

    fn from_column(column: &ColumnVector<TEntry, N>) -> Self {
        column.clone()
    }
}

impl<TEntry: Ring, const R: usize, const C: usize> Vector<TEntry, { R * C }>
    for Matrix<TEntry, R, C>
where
    [(); R * C]:,
    If<{ C != 1 }>: True,
{
    fn to_column(&self) -> ColumnVector<TEntry, { R * C }> {
        let mut entries = array::from_fn(|_| TEntry::zero());
        for r in 0..R {
            for c in 0..C {
                entries[r * C + c] = self.entries[r][c].clone();
            }
        }
        ColumnVector::v_new(entries)
    }
    fn vec_zero() -> Self {
        Self::new(array::from_fn(|_| {
            array::from_fn(|_| TEntry::zero())
        }))
    }

    fn from_column(column: &ColumnVector<TEntry, { R * C }>) -> Self {
        let mut entries = array::from_fn(|_| array::from_fn(|_| TEntry::zero()));
        for (r, row) in entries.iter_mut().enumerate() {
            for (c, entry) in row.iter_mut().enumerate() {
                *entry = column.entries[r * C + c][0].clone();
            }
        }
        Self::new(entries)
    }
}

trait CrossVal<TEntry: Ring + Value, const D: usize> {
    fn try_cross(&self, rhs: &dyn Value, op: Op) -> Option<Box<dyn Value>>;
}

impl<TEntry: Ring + Value, const D: usize, T: Value + Vector<TEntry, D>> CrossVal<TEntry, D> for T {
    default fn try_cross(&self, _rhs: &dyn Value, _op: Op) -> Option<Box<dyn Value>> {
        None
    }
}

impl<TEntry: Ring + Value, T: Vector<TEntry, 3> + Value> CrossVal<TEntry, 3> for T {
    fn try_cross(&self, rhs: &dyn Value, op: Op) -> Option<Box<dyn Value>> {
        if op == Op::Cross && rhs.get_type() == self.get_type() {
            let rhs = rhs.downcast::<T>().expect("Downcast error");
            Some(Box::new(self.cross(rhs)))
        } else {
            None
        }
    }
}

pub fn try_vector_ops<TEntry: Ring + Value, const DIM: usize, T: Vector<TEntry, DIM> + Value>(
    lhs: &T,
    rhs: &dyn Value,
    op: Op,
) -> Option<Box<dyn Value>> {
    if let Some(res) = lhs.try_cross(rhs, op) {
        return Some(res);
    }
    if rhs.get_type() == lhs.get_type() {
        let rhs = rhs.downcast::<T>().expect("Downcast error");
        Some(match op {
            Op::Add => Box::new(lhs.clone() + rhs.clone()),
            Op::Sub => Box::new(lhs.clone() - rhs.clone()),
            Op::Dot => Box::new(lhs.dot(rhs)),
            _ => return None,
        })
    } else if rhs.get_type() == TEntry::zero().get_type() {
        let rhs = rhs.downcast::<TEntry>().expect("Downcast error");
        Some(Box::new(match op {
            Op::Mul => lhs.clone() * rhs.clone(),
            _ => return None,
        }))
    } else {
        None
    }
}

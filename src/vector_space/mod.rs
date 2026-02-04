use std::{
    array, ops::{Add, Mul, Sub}
};

use itertools::Itertools;

use crate::{
    debug_multi::DebugMulti,
    matrix::{Column, Matrix, SizedMatrix, UnsizedMatrix},
    repl::{Downcast, Op, Value},
    ring_field::{Convenient, Field, QuadraticClosure, Ring},
};

pub mod subspace;

#[allow(unused)]
pub trait Vector<TEntry: Ring>:
    Add<Output = Self> + Sub<Output = Self> + Mul<TEntry, Output = Self> + Convenient
{
    fn to_vec(&self) -> Vec<TEntry>;
    fn from_vec(vec: Vec<TEntry>) -> Self;
    fn vec_zero(s: usize) -> Self;
    fn dot(&self, other: &Self) -> TEntry {
        let col_self = self.to_vec();
        let col_other = other.to_vec();
        let mut res = TEntry::zero();
        for (s, o) in col_self.iter().zip(col_other.iter()) {
            res = res + (s.clone() * o.clone());
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
    fn dimension(&self) -> usize;
}

pub trait Cross<TEntry> {
    fn cross(&self, other: &Self) -> Self
    where
        Self: Sized;
}

fn cross_arr<TEntry: Ring>([[u1, u2, u3], [v1, v2, v3]]: [[&TEntry; 3]; 2]) -> [TEntry; 3] {
    [
        u2.clone() * v3.clone() - u3.clone() * v2.clone(),
        u3.clone() * v1.clone() - u1.clone() * v3.clone(),
        u1.clone() * v2.clone() - u2.clone() * v1.clone(),
    ]
}

trait Dim3<TEntry: Ring>: Vector<TEntry> {}
impl<TEntry: Ring> Dim3<TEntry> for UnsizedMatrix<TEntry> {}
impl<TEntry: Ring> Dim3<TEntry> for SizedMatrix<TEntry, 3, 1> {}

impl<TEntry: Ring, T: Dim3<TEntry>> Cross<TEntry> for T {
    fn cross(&self, other: &Self) -> Self
    where
        Self: Sized,
    {
        if self.dimension() != 3 {
            panic!("Wrong dimension")
        }
        if let [u1, u2, u3] = self.to_vec().as_slice()
            && let [v1, v2, v3] = other.to_vec().as_slice()
        {
            Self::from_vec(cross_arr([[u1, u2, u3], [v1, v2, v3]]).to_vec())
        } else {
            unreachable!("yo what")
        }
    }
}

impl<TEntry: Ring> Vector<TEntry> for UnsizedMatrix<TEntry> {
    fn vec_zero(s: usize) -> Self {
        Self::v_new((0..s).map(|_| TEntry::zero()).collect_vec())
    }

    fn from_vec(vec: Vec<TEntry>) -> Self {
        Self::v_new(vec)
    }

    fn dimension(&self) -> usize {
        self.size().0 * self.size().1
    }
    
    fn to_vec(&self) -> Vec<TEntry> {
        self.as_vec()
    }
}

impl<TEntry: Ring, const R: usize, const C: usize> Vector<TEntry> for SizedMatrix<TEntry, R, C> {
    fn vec_zero(s: usize) -> Self {
        assert_eq!(R*C, s);
        Self::new(array::from_fn(|_|array::from_fn(|_|TEntry::zero())))
    }

    fn from_vec(vec: Vec<TEntry>) -> Self {
        Self::from_unsized(UnsizedMatrix::new(vec, (R,C))).unwrap()
    }

    fn dimension(&self) -> usize {
        R*C
    }
    
    fn to_vec(&self) -> Vec<TEntry> {
        self.rows().flatten().cloned().collect()
    }
}

trait CrossVal<TEntry: Ring + Value> {
    fn try_cross(&self, rhs: &dyn Value, op: Op) -> Option<Box<dyn Value>>;
}

impl<TEntry: Ring + Value, T: Value + Vector<TEntry>> CrossVal<TEntry> for T {
    default fn try_cross(&self, _rhs: &dyn Value, _op: Op) -> Option<Box<dyn Value>> {
        None
    }
}

impl<TEntry: Ring + Value, T: Vector<TEntry> + Value + Cross<TEntry>> CrossVal<TEntry> for T {
    fn try_cross(&self, rhs: &dyn Value, op: Op) -> Option<Box<dyn Value>> {
        if op == Op::Cross && rhs.get_type() == self.get_type() {
            let rhs = rhs.downcast::<T>().expect("Downcast error");
            Some(Box::new(self.cross(rhs)))
        } else {
            None
        }
    }
}

pub fn try_vector_ops<TEntry: Ring + Value, T: Vector<TEntry> + Value>(
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

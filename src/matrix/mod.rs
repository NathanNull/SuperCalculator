use std::{
    array,
    collections::HashMap,
    ops::{Add, Mul, Sub},
};

use itertools::Itertools;
use rand::rngs::ThreadRng;

use crate::{
    augmented_matrix::AugmentedMatrix,
    debug_multi::DebugMulti,
    expression::function::{Function, VARS},
    repl::{Downcast, Op, Value, ValueType},
    ring_field::{Convenient, Field, Ring, RingOps},
    vector_space::{
        Vector,
        subspace::{Basis, Subspace},
        try_vector_ops,
    },
};

mod eigen;
mod ops;
mod row_reduction;
pub use row_reduction::RowReduction;

pub trait Matrix<TEntry: Ring>:
    Convenient + Add<Output = Self> + Sub<Output = Self> + Mul<TEntry, Output = Self>
{
    fn set_entry(&mut self, row: usize, col: usize, entry: TEntry);
    fn get_entry(&self, row: usize, col: usize) -> Option<&TEntry>;
    fn get_mut_entry(&mut self, row: usize, col: usize) -> Option<&mut TEntry>;
    fn size(&self) -> (usize, usize);

    type Transpose: Matrix<TEntry>;
    fn transpose(&self) -> Self::Transpose;

    fn columns(&self) -> Vec<Vec<TEntry>>;
    fn new_columns(columns: Vec<Vec<TEntry>>) -> Self;

    fn scale(&mut self, scalar: TEntry) {
        let (rows, cols) = self.size();
        for row in 0..rows {
            for col in 0..cols {
                let e = self.get_mut_entry(row, col).expect("Valid index");
                *e = e.clone() * scalar.clone();
            }
        }
    }

    fn as_ref<'a>(&'a self) -> RefMatrix<'a, TEntry> {
        let (rows, cols) = self.size();
        let mut entries = vec![];
        for row in 0..rows {
            let mut v_row = vec![];
            for col in 0..cols {
                v_row.extend(self.get_entry(row, col));
            }
            entries.push(v_row);
        }
        RefMatrix::new(entries)
    }

    type Cast<Entry: Ring>;
    fn cast_into<TOtherEntry: From<TEntry> + Ring>(self) -> Self::Cast<TOtherEntry>;

    fn from_unsized(inner: UnsizedMatrix<TEntry>) -> Option<Self>;
    fn to_unsized(self) -> UnsizedMatrix<TEntry>;

    fn column_space(&self) -> Basis<TEntry, UnsizedMatrix<TEntry>>
    where
        TEntry: Field,
    {
        Subspace::new(
            self.columns()
                .into_iter()
                .map(|c| UnsizedMatrix::new(c.clone(), (c.len(), 1)))
                .collect(),
        )
        .basis()
    }

    fn rank(&self) -> usize
    where
        TEntry: Field,
    {
        self.column_space().dimension()
    }

    fn nullspace(&self) -> Basis<TEntry, UnsizedMatrix<TEntry>>
    where
        TEntry: Field,
    {
        let lhs = self.clone().to_unsized();
        let rhs = UnsizedMatrix::new(vec![TEntry::zero(); self.size().0], (self.size().0, 1));
        let sol = AugmentedMatrix::new(lhs, rhs)
            .solve()
            .unwrap()
            .gen_parametric_form(
                (0..self.size().0)
                    .map(|i| VARS[i..=i].to_string())
                    .collect(),
                vec!["1".to_string()],
            )
            .unwrap()
            .into_iter()
            .map(|f| f.eval(&HashMap::from_iter([("1".to_string(), Function::unit())])))
            .collect_vec();
        let mut vars = vec![(); self.size().0]
            .into_iter()
            .map(|_| "".to_string())
            .collect_vec();
        let mut n = 0;
        for v in sol.iter().flat_map(|s| s.variables()) {
            if !vars.contains(&v) {
                vars[n] = v;
                n += 1;
            }
        }
        Subspace::new(
            vars.iter()
                .map(|var| {
                    UnsizedMatrix::new(
                        sol.iter()
                            .map(|v| {
                                let a = v.eval(&HashMap::from_iter(vars.iter().map(|tvar| {
                                    (
                                        tvar.clone(),
                                        if tvar == var {
                                            Function::Variable(tvar.clone())
                                        } else {
                                            Function::Constant(TEntry::zero())
                                        },
                                    )
                                })));
                                let a_str = format!("{a:?}");
                                if let Function::Product(box1, box2) = a {
                                    match (*box1, *box2) {
                                        (Function::Constant(c), Function::Variable(v))
                                        | (Function::Variable(v), Function::Constant(c))
                                            if v == *var =>
                                        {
                                            c
                                        }

                                        _ => panic!("Unrecognized form {a_str}"),
                                    }
                                } else if a == Function::Constant(TEntry::zero()) {
                                    TEntry::zero()
                                } else if a == Function::Variable(var.clone()) {
                                    TEntry::one()
                                } else {
                                    panic!("Unrecognized form {a_str}");
                                }
                            })
                            .collect_vec(),
                        (self.size().0, 1),
                    )
                })
                .collect_vec(),
        )
        .basis()
    }

    fn nullity(&self) -> usize
    where
        TEntry: Field,
    {
        self.nullspace().dimension()
    }

    fn row(&self, row: usize) -> impl Iterator<Item = &TEntry> {
        (0..self.size().1).filter_map(move |col| self.get_entry(row, col))
    }

    fn rows(&self) -> impl Iterator<Item = impl Iterator<Item = &TEntry>> {
        (0..self.size().0).map(|r| self.row(r))
    }

    fn iter(&self) -> impl Iterator<Item = &TEntry> {
        self.rows().flatten()
    }
}

pub trait Column<TEntry: Ring>: Matrix<TEntry> {
    fn as_vec(&self) -> Vec<TEntry> {
        self.iter().cloned().collect_vec()
    }

    fn v_new(entries: Vec<TEntry>) -> Self {
        let elen = entries.len();
        Self::from_unsized(UnsizedMatrix::new(
            entries.into_iter().collect_vec(),
            (elen, 1),
        ))
        .expect("Should be the right size")
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct UnsizedMatrix<TEntry: Ring> {
    entries: Vec<TEntry>,
    size: (usize, usize),
}

impl<TEntry: Ring + DebugMulti> Matrix<TEntry> for UnsizedMatrix<TEntry> {
    fn set_entry(&mut self, row: usize, col: usize, entry: TEntry) {
        *self
            .entries
            .get_mut(row * self.size.1 + col)
            .expect("Invalid matrix index") = entry;
    }

    fn get_entry(&self, row: usize, col: usize) -> Option<&TEntry> {
        self.entries.get(row * self.size.1 + col)
    }

    fn get_mut_entry(&mut self, row: usize, col: usize) -> Option<&mut TEntry> {
        self.entries.get_mut(row * self.size.1 + col)
    }

    fn size(&self) -> (usize, usize) {
        self.size
    }

    type Transpose = Self;
    fn transpose(&self) -> Self::Transpose {
        let mut t = vec![];
        for c in 0..self.size.1 {
            for r in 0..self.size.0 {
                t.push(self.get_entry(r, c).expect("valid index").clone());
            }
        }
        Self::new(t, (self.size.1, self.size.0))
    }

    fn columns(&self) -> Vec<Vec<TEntry>> {
        self.transpose()
            .entries
            .iter()
            .chunks(self.size.1)
            .into_iter()
            .map(|col| col.cloned().collect())
            .collect()
    }
    fn new_columns(columns: Vec<Vec<TEntry>>) -> Self {
        Self::new(
            columns.iter().flatten().cloned().collect_vec(),
            (
                columns.first().map(|v| v.len()).unwrap_or_default(),
                columns.len(),
            ),
        )
    }

    type Cast<Entry: Ring> = UnsizedMatrix<Entry>;
    fn cast_into<TOtherEntry: From<TEntry> + Ring>(self) -> Self::Cast<TOtherEntry> {
        let entries = self
            .entries
            .into_iter()
            .map(|v| TOtherEntry::from(v))
            .collect();
        UnsizedMatrix::new(entries, self.size)
    }

    fn from_unsized(inner: UnsizedMatrix<TEntry>) -> Option<Self> {
        Some(inner)
    }
    fn to_unsized(self) -> UnsizedMatrix<TEntry> {
        self
    }
}

impl<TEntry: Ring> UnsizedMatrix<TEntry> {
    pub fn fill_size(fill: impl Fn(usize, usize) -> TEntry, size: (usize, usize)) -> Self {
        let mut entries = vec![];
        for row in 0..size.0 {
            for col in 0..size.1 {
                entries.push(fill(row, col))
            }
        }
        Self::new(entries, size)
    }
    pub fn new(entries: Vec<TEntry>, size: (usize, usize)) -> Self {
        (entries.len() == size.0 * size.1)
            .then(|| Self { entries, size })
            .expect("Incorrect entries")
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct SizedMatrix<TEntry: Ring, const R: usize, const C: usize> {
    inner: UnsizedMatrix<TEntry>,
}

impl<TEntry: Ring, const R: usize, const C: usize> Matrix<TEntry> for SizedMatrix<TEntry, R, C> {
    fn set_entry(&mut self, row: usize, col: usize, entry: TEntry) {
        self.inner.set_entry(row, col, entry);
    }

    fn get_entry(&self, row: usize, col: usize) -> Option<&TEntry> {
        self.inner.get_entry(row, col)
    }

    fn get_mut_entry(&mut self, row: usize, col: usize) -> Option<&mut TEntry> {
        self.inner.get_mut_entry(row, col)
    }

    fn size(&self) -> (usize, usize) {
        (R, C)
    }

    type Transpose = SizedMatrix<TEntry, C, R>;
    fn transpose(&self) -> Self::Transpose {
        SizedMatrix::from_unsized(self.inner.transpose()).expect("Should be the right size")
    }

    fn columns(&self) -> Vec<Vec<TEntry>> {
        self.inner.columns()
    }
    fn new_columns(columns: Vec<Vec<TEntry>>) -> Self {
        Self::from_unsized(UnsizedMatrix::new_columns(columns)).expect("right size")
    }

    type Cast<Entry: Ring> = SizedMatrix<Entry, R, C>;
    fn cast_into<TOtherEntry: From<TEntry> + Ring>(self) -> Self::Cast<TOtherEntry> {
        SizedMatrix::from_unsized(self.inner.cast_into()).expect("Should be the right size")
    }

    fn from_unsized(inner: UnsizedMatrix<TEntry>) -> Option<Self> {
        (inner.size == (R, C)).then(|| Self { inner })
    }
    fn to_unsized(self) -> UnsizedMatrix<TEntry> {
        self.inner
    }
}

impl<TEntry: Ring, const R: usize> Column<TEntry> for SizedMatrix<TEntry, R, 1> {}
impl<TEntry: Ring> Column<TEntry> for UnsizedMatrix<TEntry> {}

/// Example: ```matrix!(1,2,3;4,5,6;7,8,9)```
#[macro_export]
macro_rules! matrix {
    ($( $( $num:literal $(/$den:literal)? ),+ );+ ) => {
        $crate::matrix::SizedMatrix::new([ $( [ $( {
            r!($num $(/$den)?)
        } ),* ] ),* ])
    };
}

#[macro_export]
macro_rules! fmatrix {
    ($( $( $num:expr ),+ );+ ) => {
        $crate::matrix::SizedMatrix::new([ $( [ $( {
            $crate::num::real::Real($num as f64)
        } ),* ] ),* ])
    };
}

#[macro_export]
macro_rules! zmatrix {
    (<$n: literal> $( $( $num:literal ),+ );+) => {
        $crate::matrix::SizedMatrix::new([ $( [ $( {
            ZMod::<$n>::new($num as usize)
        } ),* ] ),* ])
    };
}

impl<TEntry: Ring, const R: usize, const C: usize> SizedMatrix<TEntry, R, C> {
    pub fn new(entries: [[TEntry; C]; R]) -> Self {
        Self {
            inner: UnsizedMatrix::new(entries.into_iter().flatten().collect(), (R, C)),
        }
    }

    pub fn new_columns(columns: [[TEntry; R]; C]) -> Self {
        SizedMatrix::new(columns).transpose()
    }
}

impl<TEntry: Ring, const R: usize, const C: usize> std::fmt::Debug for SizedMatrix<TEntry, R, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_ref().fmt(f)
    }
}

impl<TEntry: Ring> std::fmt::Debug for UnsizedMatrix<TEntry> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_ref().fmt(f)
    }
}

pub type ColumnVector<TEntry, const N: usize> = SizedMatrix<TEntry, N, 1>;
impl<TEntry: Ring, const N: usize> ColumnVector<TEntry, N> {
    pub fn as_array(&self) -> [TEntry; N] {
        self.inner
            .entries
            .iter()
            .cloned()
            .array_chunks()
            .next()
            .unwrap()
    }
}

pub type SquareMatrix<TEntry, const N: usize> = SizedMatrix<TEntry, N, N>;
impl<TEntry: Ring, const N: usize> SquareMatrix<TEntry, N> {
    #[allow(unused)]
    pub fn determinant(&self) -> TEntry {
        self.as_ref().determinant()
    }

    pub fn ident() -> Self {
        let mut me = Self::from_unsized(UnsizedMatrix {
            entries: vec![TEntry::zero(); N * N],
            size: (N, N),
        })
        .expect("Should be the right size");
        for r in 0..N {
            me.set_entry(r, r, TEntry::one());
        }
        me
    }
}

impl<TEntry: Ring, const N: usize> Ring for SquareMatrix<TEntry, N> {
    fn try_inverse(&self) -> Option<Self> {
        if let Some(inv) = self.determinant().try_inverse() {
            if let Some(aug) = AugmentedMatrix::new(self.clone(), Self::ident()).solve() {
                if aug.left_matrix == Self::ident() {
                    Some(aug.right_matrix)
                } else {
                    unreachable!("Determinant was invertible but matrix reduces to non-identity")
                }
            } else {
                let unsizedmat = self.as_ref();
                let c: SquareMatrix<TEntry, N> = SquareMatrix::new(array::from_fn(|r| {
                    array::from_fn(|c| unsizedmat.cofactor(c, r)) // transpose
                }));
                Some(c * inv)
            }
        } else {
            None
        }
    }

    fn negate(&self) -> Self {
        let mut neg = self.clone();
        neg.scale(TEntry::one().negate());
        neg
    }

    fn zero() -> Self {
        Self::new(array::from_fn(|_| array::from_fn(|_| TEntry::one())))
    }

    fn one() -> Self {
        Self::ident()
    }

    fn generate(rng: &mut ThreadRng, basic: bool) -> Self {
        Self::new(array::from_fn(|_| {
            array::from_fn(|_| TEntry::generate(rng, basic))
        }))
    }
}

// TODO: Should square matrices be an exponential ring? The square matrix exponential operation is defined but in its canonical form it's an infinite series.

pub struct RefMatrix<'a, TEntry> {
    size: (usize, usize),
    entries: Vec<Vec<&'a TEntry>>,
}
impl<'a, TEntry: Ring> RefMatrix<'a, TEntry> {
    pub fn new(entries: Vec<Vec<&'a TEntry>>) -> Self {
        let empty = vec![];
        let size = (entries.len(), entries.first().unwrap_or(&empty).len());
        for entry in &entries {
            assert_eq!(entry.len(), size.1, "Matrix rows must be the same size");
        }
        Self { entries, size }
    }

    pub fn determinant(&self) -> TEntry {
        assert_eq!(
            self.size.0, self.size.1,
            "Can't take the determinant of a non-square matrix"
        );
        if self.size.1 == 1 {
            // 1x1 case, just its own value
            return self.entries[0][0].clone();
        }
        let mut res = TEntry::zero();
        for col in 0..self.size.0 {
            let cofactor = self.cofactor(0, col);
            res = res + cofactor * self.entries[0][col].clone();
        }
        res
    }

    pub fn cofactor(&self, r: usize, c: usize) -> TEntry {
        assert!(
            r < self.size.0 && c < self.size.1,
            "Row {r} and column {c} must be within size of matrix {:?}",
            self.size
        );

        let mut sign = TEntry::one();
        if (r + c) % 2 == 1 {
            sign = sign.negate();
        }

        let mut entries = vec![];
        for rp in 0..self.size.0 {
            if rp == r {
                continue;
            }
            let row = &self.entries[rp];
            let mut m_row = vec![];
            for (cp, rcp) in row.iter().enumerate() {
                if cp == c {
                    continue;
                }
                m_row.push(*rcp);
            }
            entries.push(m_row);
        }
        RefMatrix::new(entries).determinant() * sign
    }
}

impl<TEntry: Ring> std::fmt::Debug for RefMatrix<'_, TEntry> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res: Vec<Vec<Vec<String>>> = self
            .entries
            .iter()
            .map(|r| r.iter().map(|c| c.lines()).collect())
            .collect();
        let max_len = res.iter().fold(0, |acc, row| {
            acc.max(row.iter().fold(0, |a2, s| {
                a2.max(s.iter().fold(0, |a3, s2| a3.max(s2.len())))
            }))
        });
        let lines = (*self.entries[0][0]).lines().len();
        for (r, row) in res.iter().enumerate() {
            for l in 0..lines {
                write!(
                    f,
                    "{}",
                    match r * lines + l {
                        0 if self.size.0 == 1 && lines == 1 => "(",
                        0 => "╭",
                        n if n == self.size.0 * lines - 1 => "\r\n╰",
                        _ => "\r\n│",
                    }
                )?;
                for cell in row {
                    let entry = &cell[l];
                    let spaces = " ".repeat(max_len - entry.len());
                    write!(f, " {entry}{spaces} ")?;
                }
                write!(
                    f,
                    "{}",
                    match r * lines + l {
                        0 if self.size.0 == 1 && lines == 1 => ")",
                        0 => "╮",
                        n if n == self.size.0 * lines - 1 => "╯",
                        _ => "│",
                    }
                )?;
            }
        }
        Ok(())
    }
}

impl<TEntry: Ring + Value> Value for UnsizedMatrix<TEntry> {
    fn get_type(&self) -> ValueType {
        let size = self.size();
        ValueType::Matrix(Box::new(TEntry::zero().get_type()), size.0, size.1)
    }

    fn try_op(
        &self,
        op: Op,
        rhs: Box<dyn Value>,
    ) -> Result<Box<dyn Value>, Box<dyn std::error::Error>> {
        if op == Op::Mul
            && let ValueType::Matrix(rentry, rr, _) = rhs.get_type()
            && let ValueType::Matrix(lentry, _, lc) = self.get_type()
            && rentry == lentry
            && lc == rr
            && let Some(res) = rhs.downcast::<UnsizedMatrix<TEntry>>()
        {
            return Ok(Box::new(self.clone() * res.clone()));
        }
        self.try_ring_ops(&*rhs, op)
            .or_else(|| try_vector_ops::<TEntry, _>(self, &*rhs, op))
            .ok_or_else(|| "Invalid matrix operation".into())
    }
}

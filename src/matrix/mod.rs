use std::{array, collections::HashMap};

use rand::rngs::ThreadRng;

use crate::{
    augmented_matrix::AugmentedMatrix,
    debug_multi::DebugMulti,
    expression::function::{Function, VARS},
    if_trait::{If, True},
    repl::{Op, Value, ValueType},
    ring_field::{Field, Ring, try_ring_ops},
    vector_space::{
        Vector,
        subspace::{Basis, Subspace},
        try_vector_ops,
    },
};

mod eigen;
mod ops;
mod row_reduction;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Matrix<TEntry: Ring, const R: usize, const C: usize> {
    pub entries: [[TEntry; C]; R],
}

/// Example: ```matrix!(1,2,3;4,5,6;7,8,9)```
#[macro_export]
macro_rules! matrix {
    ($( $( $num:literal $(/$den:literal)? ),+ );+ ) => {
        $crate::matrix::Matrix::new([ $( [ $( {
            r!($num $(/$den)?)
        } ),* ] ),* ])
    };
}

#[macro_export]
macro_rules! fmatrix {
    ($( $( $num:expr ),+ );+ ) => {
        $crate::matrix::Matrix::new([ $( [ $( {
            $crate::num::real::Real($num as f64)
        } ),* ] ),* ])
    };
}

#[macro_export]
macro_rules! zmatrix {
    (<$n: literal> $( $( $num:literal ),+ );+) => {
        $crate::matrix::Matrix::new([ $( [ $( {
            ZMod::<$n>::new($num as usize)
        } ),* ] ),* ])
    };
}

impl<TEntry: Ring, const R: usize, const C: usize> Matrix<TEntry, R, C> {
    pub const fn new(entries: [[TEntry; C]; R]) -> Self {
        Self { entries }
    }

    pub fn new_columns(columns: [[TEntry; R]; C]) -> Self {
        Matrix::new(columns).transpose()
    }

    pub fn transpose(&self) -> Matrix<TEntry, C, R> {
        let mut t = Matrix::new(array::from_fn(|_| {
            array::from_fn(|_| TEntry::zero())
        }));
        for r in 0..R {
            for c in 0..C {
                t.entries[c][r] = self.entries[r][c].clone();
            }
        }
        t
    }

    pub fn columns(&self) -> [ColumnVector<TEntry, R>; C] {
        self.transpose().entries.map(|col| ColumnVector::v_new(col))
    }

    pub fn scale(&mut self, scalar: TEntry) {
        for r in 0..R {
            for c in 0..C {
                self.entries[r][c] = self.entries[r][c].clone() * scalar.clone();
            }
        }
    }

    pub fn as_unsized<'a>(&'a self) -> UnsizedMatrix<'a, TEntry> {
        let mut entries = vec![];
        for row in &self.entries {
            let mut v_row = vec![];
            for val in row {
                v_row.push(val);
            }
            entries.push(v_row);
        }
        UnsizedMatrix::new(entries)
    }

    pub fn cast_into<TOtherEntry: From<TEntry> + Ring>(self) -> Matrix<TOtherEntry, R, C> {
        let entries = self.entries.map(|row| row.map(|v| TOtherEntry::from(v)));
        Matrix { entries }
    }

    pub fn column_space(&self) -> Basis<TEntry, R, ColumnVector<TEntry, R>>
    where
        TEntry: Field,
    {
        Subspace::new(self.columns()).basis()
    }

    pub fn rank(&self) -> usize
    where
        TEntry: Field,
    {
        self.column_space().dimension()
    }

    pub fn nullspace(&self) -> Basis<TEntry, C, ColumnVector<TEntry, C>>
    where
        TEntry: Field,
    {
        let sol = AugmentedMatrix::new(self.clone(), ColumnVector::vec_zero())
            .solve()
            .unwrap()
            .gen_parametric_form(
                array::from_fn(|i| VARS[i..=i].to_string()),
                ["1".to_string()],
            )
            .unwrap()
            .map(|f| f.eval(&HashMap::from_iter([("1".to_string(), Function::unit())])));
        let mut vars = [(); R].map(|_| "".to_string());
        let mut n = 0;
        for v in sol.iter().flat_map(|s| s.variables()) {
            if !vars.contains(&v) {
                vars[n] = v;
                n += 1;
            }
        }
        Subspace::new(vars.each_ref().map(|var| {
            ColumnVector::v_new(sol.each_ref().map(|v| {
                let a = v.eval(&HashMap::from_iter(vars.each_ref().map(|tvar| {
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
            }))
        }))
        .basis()
    }

    pub fn nullity(&self) -> usize
    where
        TEntry: Field,
    {
        self.nullspace().dimension()
    }
}

impl<TEntry: Ring, const R: usize, const C: usize> std::fmt::Debug for Matrix<TEntry, R, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_unsized().fmt(f)
    }
}

pub type ColumnVector<TEntry, const N: usize> = Matrix<TEntry, N, 1>;
impl<TEntry: Ring, const N: usize> ColumnVector<TEntry, N> {
    pub fn v_new(entries: [TEntry; N]) -> Self {
        Self {
            entries: entries.map(|r| [r]),
        }
    }

    pub fn as_array(&self) -> [TEntry; N] {
        self.entries.each_ref().map(|r| r[0].clone())
    }
}

pub type SquareMatrix<TEntry, const N: usize> = Matrix<TEntry, N, N>;
impl<TEntry: Ring, const N: usize> SquareMatrix<TEntry, N> {
    #[allow(unused)]
    pub fn determinant(&self) -> TEntry {
        self.as_unsized().determinant()
    }

    pub fn ident() -> Self {
        let mut me = Self::new(array::from_fn(|_| {
            array::from_fn(|_| TEntry::zero())
        }));
        for (r, row) in me.entries.iter_mut().enumerate() {
            row[r] = TEntry::one()
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
                let unsizedmat = self.as_unsized();
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
        Self::new(array::from_fn(|_| {
            array::from_fn(|_| TEntry::one())
        }))
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

pub struct UnsizedMatrix<'a, TEntry> {
    size: (usize, usize),
    entries: Vec<Vec<&'a TEntry>>,
}
impl<'a, TEntry: Ring> UnsizedMatrix<'a, TEntry> {
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
        UnsizedMatrix::new(entries).determinant() * sign
    }
}

impl<TEntry: Ring> std::fmt::Debug for UnsizedMatrix<'_, TEntry> {
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

// Specialization gaming
// Thanks I hate it
trait MatVecOp {
    fn vec_op(&self, op: Op, rhs: &dyn Value)
    -> Result<Box<dyn Value>, Box<dyn std::error::Error>>;
}
trait MatRingOp {
    fn ring_op(
        &self,
        op: Op,
        rhs: &dyn Value,
    ) -> Result<Box<dyn Value>, Box<dyn std::error::Error>>;
}

impl<TEntry: Ring + Value, const R: usize, const C: usize> MatVecOp for Matrix<TEntry, R, C> {
    default fn vec_op(
        &self,
        _op: Op,
        _rhs: &dyn Value,
    ) -> Result<Box<dyn Value>, Box<dyn std::error::Error>> {
        Err("Invalid matrix operation".into())
    }
}
impl<TEntry: Ring + Value, const R: usize, const C: usize> MatRingOp for Matrix<TEntry, R, C> {
    default fn ring_op(
        &self,
        _op: Op,
        _rhs: &dyn Value,
    ) -> Result<Box<dyn Value>, Box<dyn std::error::Error>> {
        Err("Invalid matrix operation".into())
    }
}
impl<TEntry: Ring + Value, const R: usize, const C: usize> MatVecOp for Matrix<TEntry, R, C>
where
    [(); R * C]:,
    If<{ C != 1 }>: True,
{
    fn vec_op(
        &self,
        op: Op,
        rhs: &dyn Value,
    ) -> Result<Box<dyn Value>, Box<dyn std::error::Error>> {
        try_vector_ops(self, rhs, op).ok_or_else(|| "Invalid matrix operation".into())
    }
}
impl<TEntry: Ring + Value, const N: usize> MatVecOp for ColumnVector<TEntry, N> {
    fn vec_op(
        &self,
        op: Op,
        rhs: &dyn Value,
    ) -> Result<Box<dyn Value>, Box<dyn std::error::Error>> {
        try_vector_ops(self, rhs, op).ok_or_else(|| "Invalid matrix operation".into())
    }
}

impl<TEntry: Ring + Value, const N: usize> MatRingOp for SquareMatrix<TEntry, N> {
    fn ring_op(
        &self,
        op: Op,
        rhs: &dyn Value,
    ) -> Result<Box<dyn Value>, Box<dyn std::error::Error>> {
        try_ring_ops(self, rhs, op).ok_or_else(|| "Invalid matrix operation".into())
    }
}

impl<TEntry: Ring + Value, const R: usize, const C: usize> Value for Matrix<TEntry, R, C> {
    fn get_type(&self) -> ValueType {
        ValueType::Matrix(R, C)
    }

    fn try_op(
        &self,
        op: Op,
        rhs: Box<dyn Value>,
    ) -> Result<Box<dyn Value>, Box<dyn std::error::Error>> {
        self.vec_op(op, &*rhs).or_else(|_| self.ring_op(op, &*rhs))
    }
}

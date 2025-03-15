use std::array;

use rand::rngs::ThreadRng;

use crate::{
    augmented_matrix::AugmentedMatrix,
    debug_multi::DebugMulti,
    ring_field::{Field, Ring}, vector_space::span::{Span, Basis},
};

mod ops;
mod row_reduction;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Matrix<TEntry: Ring, const R: usize, const C: usize> {
    pub entries: [[TEntry; C]; R],
}

impl<TEntry: Ring, const R: usize, const C: usize> Matrix<TEntry, R, C> {
    pub fn new(entries: [[TEntry; C]; R]) -> Self {
        Self { entries }
    }

    pub fn new_columns(columns: [[TEntry; R]; C]) -> Self {
        Matrix::new(columns).transpose()
    }

    pub fn transpose(&self) -> Matrix<TEntry, C, R> {
        let mut t = Matrix::new([[TEntry::additive_ident(); R]; C]);
        for r in 0..R {
            for c in 0..C {
                t.entries[c][r] = self.entries[r][c];
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
                self.entries[r][c] = self.entries[r][c] * scalar;
            }
        }
    }

    pub fn into_unsized<'a>(&'a self) -> UnsizedMatrix<'a, TEntry> {
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

    pub fn column_space(&self) -> Basis<TEntry, R, ColumnVector<TEntry, R>> where TEntry: Field {
        Span::new(self.columns()).basis()
    }

    pub fn rank(&self) -> usize where TEntry: Field {
        self.column_space().dimension()
    }
}

impl<TEntry: Ring, const R: usize, const C: usize> std::fmt::Debug for Matrix<TEntry, R, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.into_unsized().fmt(f)
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
        self.entries.map(|r| r[0])
    }
}

pub type SquareMatrix<TEntry, const N: usize> = Matrix<TEntry, N, N>;
impl<TEntry: Ring, const N: usize> SquareMatrix<TEntry, N> {
    #[allow(unused)]
    pub fn determinant(&self) -> TEntry {
        self.into_unsized().determinant()
    }

    pub fn ident() -> Self {
        let mut me = Self::new([[TEntry::additive_ident(); N]; N]);
        for (r, row) in me.entries.iter_mut().enumerate() {
            row[r] = TEntry::multiplicative_ident()
        }
        me
    }
}

impl<TEntry: Ring, const N: usize> Ring for SquareMatrix<TEntry, N> {
    fn try_inverse(&self) -> Option<Self> {
        if let Some(aug) = AugmentedMatrix::new(*self, Self::ident()).solve() {
            if aug.left_matrix == Self::ident() {
                Some(aug.right_matrix)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn negate(&self) -> Self {
        let mut neg = *self;
        neg.scale(TEntry::multiplicative_ident().negate());
        neg
    }

    fn additive_ident() -> Self {
        Self::new([[TEntry::multiplicative_ident(); N]; N])
    }

    fn multiplicative_ident() -> Self {
        Self::ident()
    }

    fn generate(rng: &mut ThreadRng) -> Self {
        Self::new(array::from_fn(|_| {
            array::from_fn(|_| TEntry::generate(rng))
        }))
    }
}

pub struct UnsizedMatrix<'a, TEntry> {
    size: (usize, usize),
    entries: Vec<Vec<&'a TEntry>>,
}
impl<'a, TEntry: Ring> UnsizedMatrix<'a, TEntry> {
    pub fn new(entries: Vec<Vec<&'a TEntry>>) -> Self {
        let empty = vec![];
        let size = (
            entries.len(),
            entries.get(0).unwrap_or_else(|| &empty).len(),
        );
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
            // 2x2 case, ad-bc
            return *self.entries[0][0];
        }
        let mut res = TEntry::additive_ident();
        let neg_1 = TEntry::multiplicative_ident().negate();
        let mut sign = neg_1;
        for col in 0..self.size.0 {
            sign = sign * neg_1;
            let minor = self.minor(0, col);
            let m = minor.determinant();
            res = res + (m * (sign * *self.entries[0][col]));
        }
        res
    }

    pub fn minor(&self, r: usize, c: usize) -> UnsizedMatrix<TEntry> {
        assert!(
            r < self.size.0 && c < self.size.1,
            "Row {r} and column {c} must be within size of matrix {:?}",
            self.size
        );
        let mut entries = vec![];
        for rp in 0..self.size.0 {
            if rp == r {
                continue;
            }
            let row = &self.entries[rp];
            let mut m_row = vec![];
            for cp in 0..self.size.1 {
                if cp == c {
                    continue;
                }
                m_row.push(row[cp]);
            }
            entries.push(m_row);
        }
        UnsizedMatrix::new(entries)
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
        for r in 0..self.size.0 {
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
                for c in 0..self.size.1 {
                    let entry = &res[r][c][l];
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

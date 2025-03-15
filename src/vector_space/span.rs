use std::marker::PhantomData;

use crate::matrix::Matrix;

use super::*;

pub const MAX_SPAN_SIZE: usize = 32;

pub struct Span<TEntry: Field, const DIM: usize, TVec: Vector<TEntry, DIM>, const VECS: usize> {
    vectors: [TVec; VECS],
    _entry_t: PhantomData<TEntry>,
}

pub struct Basis<TEntry: Field, const DIM: usize, TVec: Vector<TEntry, DIM>> {
    vectors: Vec<TVec>,
    _entry_t: PhantomData<TEntry>,
}

impl<TEntry: Field, const DIM: usize, TVec: Vector<TEntry, DIM>, const VECS: usize>
    Span<TEntry, DIM, TVec, VECS>
{
    pub fn new(vectors: [TVec; VECS]) -> Self {
        assert!(
            vectors.len() <= MAX_SPAN_SIZE,
            "Can't have more than {MAX_SPAN_SIZE} vectors (found {})",
            vectors.len()
        );
        Self {
            vectors,
            _entry_t: PhantomData,
        }
    }

    pub fn basis(&self) -> Basis<TEntry, DIM, TVec> {
        let mut m = Matrix::new_columns(self.vectors.map(|v| v.to_column().as_array()));
        m.reduce_to_ref();
        let mut basis = vec![];
        for pivot in m.pivots() {
            basis.push(self.vectors[pivot.col]);
        }
        Basis::new(basis)
    }

    pub fn dimension(&self) -> usize {
        self.basis().vectors.len()
    }
}

impl<TEntry: Field, const DIM: usize, TVec: Vector<TEntry, DIM>> Basis<TEntry, DIM, TVec> {
    pub fn new(vectors: Vec<TVec>) -> Self {
        Self {
            vectors,
            _entry_t: PhantomData,
        }
    }

    pub fn dimension(&self) -> usize {
        self.vectors.len()
    }
}

impl<TEntry: Field, const DIM: usize, TVec: Vector<TEntry, DIM>, const VECS: usize> std::fmt::Debug
    for Span<TEntry, DIM, TVec, VECS>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", Basis::new(self.vectors.to_vec()))
    }
}

impl<TEntry: Field, const DIM: usize, TVec: Vector<TEntry, DIM>> std::fmt::Debug
    for Basis<TEntry, DIM, TVec>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res: Vec<Vec<String>> = self.vectors.iter().map(|c| c.lines()).collect();
        let max_len = res.iter().fold(0, |acc, row| {
            acc.max(row.iter().fold(0, |a2, s| a2.max(s.len())))
        });
        let lines = self.vectors[0].lines().len();
        for l in 0..lines {
            write!(
                f,
                "{}",
                match l {
                    0 if lines == 1 => "{",
                    0 => "╭",
                    n if n == lines - 1 => "\r\n╰",
                    _ => "\r\n│",
                }
            )?;
            for c in 0..self.vectors.len() {
                let entry = &res[c][l];
                let spaces = " ".repeat(max_len - entry.len());
                write!(f, " {entry}{spaces} ")?;
            }
            write!(
                f,
                "{}",
                match l {
                    0 if lines == 1 => "}",
                    0 => "╮",
                    n if n == lines - 1 => "╯",
                    _ => "│",
                }
            )?;
        }
        Ok(())
    }
}

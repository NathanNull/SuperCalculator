use std::marker::PhantomData;

use rand::rng;

use crate::matrix::Matrix;

use super::*;

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
        self.basis().dimension()
    }

    pub fn sample(&self, basic: bool) -> TVec {
        self.basis().sample(basic)
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

    pub fn sample(&self, basic: bool) -> TVec {
        let mut res = TVec::zero();
        let mut rng = rng();
        for v in &self.vectors {
            res = res + *v * TEntry::generate(&mut rng, basic);
        }
        res
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
                let comma = if c == self.vectors.len() - 1 {
                    ""
                } else if l == lines - 1 {
                    ","
                } else {
                    " "
                };
                write!(f, "{entry}{comma}")?;
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

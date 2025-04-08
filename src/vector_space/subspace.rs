use anymap::{any::Any, Map};
use rand::rng;
use std::{
    collections::HashMap,
    marker::PhantomData,
    sync::{LazyLock, Mutex},
};

use crate::{augmented_matrix::AugmentedMatrix, matrix::Matrix};

use super::*;

pub struct Subspace<TEntry: Field, const DIM: usize, TVec: Vector<TEntry, DIM>, const VECS: usize> {
    vectors: [TVec; VECS],
    _entry_t: PhantomData<TEntry>,
}

#[derive(Clone)]
pub struct Basis<TEntry: Field, const DIM: usize, TVec: Vector<TEntry, DIM>> {
    vectors: Vec<TVec>,
    _entry_t: PhantomData<TEntry>,
}

impl<TEntry: Field, const DIM: usize, TVec: Vector<TEntry, DIM>, const VECS: usize>
    Subspace<TEntry, DIM, TVec, VECS>
{
    pub fn new(vectors: [TVec; VECS]) -> Self {
        Self {
            vectors,
            _entry_t: PhantomData,
        }
    }

    pub fn basis(&self) -> Basis<TEntry, DIM, TVec> {
        static CACHE: LazyLock<Mutex<Map<dyn Any + Send + Sync>>> =
            LazyLock::new(|| Mutex::new(Map::new()));
        if let Ok(mut cache) = CACHE.try_lock() {
            let t_cache = if let Some(t_cache) =
                cache.get_mut::<HashMap<[TVec; VECS], Basis<TEntry, DIM, TVec>>>()
            {
                t_cache
            } else {
                cache.insert(HashMap::<[TVec; VECS], Basis<TEntry, DIM, TVec>>::new());
                cache.get_mut().unwrap()
            };
            if let Some(cached) = t_cache.get(&self.vectors) {
                return cached.clone();
            } else {
                let b = self.basis_raw();
                t_cache.insert(self.vectors.clone(), b.clone());
                return b;
            }
        }
        // Couldn't get cache lock so just do it normally
        self.basis_raw()
    }

    fn basis_raw(&self) -> Basis<TEntry, DIM, TVec> {
        let mut m = Matrix::new_columns(self.vectors.clone().map(|v| v.to_column().as_array()));
        m.reduce_to_ref();
        let mut basis = vec![];
        for pivot in m.pivots() {
            basis.push(self.vectors[pivot.col].clone());
        }
        Basis::new(basis)
    }

    pub fn dimension(&self) -> usize {
        self.basis().dimension()
    }

    pub fn sample(&self, basic: bool) -> TVec {
        self.basis().sample(basic)
    }

    pub fn linearly_independant(&self) -> bool {
        self.vectors.len() == self.basis().vectors.len()
    }

    pub fn contains(&self, vec: TVec) -> bool {
        let m = Matrix::new_columns(self.vectors.clone().map(|v| v.to_column().as_array()));
        if let Some(aug) = AugmentedMatrix::new(m, vec.to_column()).solve() {
            aug.consistent().unwrap()
        } else {
            false
        }
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
            res = res + v.clone() * TEntry::generate(&mut rng, basic);
        }
        res
    }
}

impl<TEntry: Field, const DIM: usize, TVec: Vector<TEntry, DIM>, const VECS: usize> std::fmt::Debug
    for Subspace<TEntry, DIM, TVec, VECS>
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
        let lines = TVec::zero().lines().len();
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
            if self.vectors.len() == 0 {
                write!(f, " ")?;
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

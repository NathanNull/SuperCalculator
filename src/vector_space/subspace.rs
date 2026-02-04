use anymap::{Map, any::Any};
use rand::rng;
use std::{
    collections::HashMap,
    marker::PhantomData,
    sync::{LazyLock, Mutex},
};

use crate::{
    augmented_matrix::AugmentedMatrix,
    matrix::{Matrix, RowReduction},
};

use super::*;

pub struct Subspace<TEntry: Field, TVec: Vector<TEntry>> {
    vectors: Vec<TVec>,
    _entry_t: PhantomData<TEntry>,
}

#[derive(Clone)]
pub struct Basis<TEntry: Field, TVec: Vector<TEntry>> {
    vectors: Vec<TVec>,
    _entry_t: PhantomData<TEntry>,
}

impl<TEntry: Field, TVec: Vector<TEntry>> Subspace<TEntry, TVec> {
    pub fn new(vectors: Vec<TVec>) -> Self {
        Self {
            vectors,
            _entry_t: PhantomData,
        }
    }

    pub fn basis(&self) -> Basis<TEntry, TVec> {
        static CACHE: LazyLock<Mutex<Map<dyn Any + Send + Sync>>> =
            LazyLock::new(|| Mutex::new(Map::new()));
        if let Ok(mut cache) = CACHE.try_lock() {
            let t_cache =
                if let Some(t_cache) = cache.get_mut::<HashMap<Vec<TVec>, Basis<TEntry, TVec>>>() {
                    t_cache
                } else {
                    cache.insert(HashMap::<Vec<TVec>, Basis<TEntry, TVec>>::new());
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

    fn basis_raw(&self) -> Basis<TEntry, TVec> {
        let mut m =
            UnsizedMatrix::new_columns(self.vectors.iter().map(|v| v.to_vec()).collect());
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
        let m = UnsizedMatrix::new_columns(self.vectors.iter().map(|v| v.to_vec()).collect());
        if let Some(aug) = AugmentedMatrix::new(m, UnsizedMatrix::v_new(vec.to_vec())).solve() {
            aug.consistent().unwrap()
        } else {
            false
        }
    }
}

impl<TEntry: Field, TVec: Vector<TEntry>> Basis<TEntry, TVec> {
    pub fn new(vectors: Vec<TVec>) -> Self {
        Self {
            vectors,
            _entry_t: PhantomData,
        }
    }

    pub fn dimension(&self) -> usize {
        self.vectors.len()
    }

    pub fn vectors(&self) -> &Vec<TVec> {
        &self.vectors
    }

    pub fn sample(&self, basic: bool) -> TVec {
        let mut res = TVec::vec_zero(self.vectors[0].dimension());
        let mut rng = rng();
        for v in &self.vectors {
            res = res + v.clone() * TEntry::generate(&mut rng, basic);
        }
        res
    }

    const MAX_DIM: usize = 32;
    pub fn contains(&self, vec: TVec) -> bool
    where
        [(); Self::MAX_DIM]:,
    {
        assert!(
            self.vectors.len() <= Self::MAX_DIM,
            "Can't operate on a basis of dimension greater than {:?}",
            Self::MAX_DIM
        );
        let b = Subspace::new(self.vectors.clone());
        b.contains(vec)
    }
}

impl<TEntry: Field, TVec: Vector<TEntry>> std::fmt::Debug for Subspace<TEntry, TVec> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", Basis::new(self.vectors.to_vec()))
    }
}

impl<TEntry: Field, TVec: Vector<TEntry>> std::fmt::Debug for Basis<TEntry, TVec> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res: Vec<Vec<String>> = self.vectors.iter().map(|c| c.lines()).collect();
        let lines = TVec::vec_zero(self.vectors[0].dimension()).lines().len();
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
            for (c, v) in res.iter().enumerate() {
                let entry = &v[l];
                let comma = if c == self.vectors.len() - 1 {
                    ""
                } else if l == lines - 1 {
                    ","
                } else {
                    " "
                };
                write!(f, "{entry}{comma}")?;
            }
            if self.vectors.is_empty() {
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

use crate::{
    debug_multi::DebugMulti, expression::{equation::Equation, function::Function}, matrix::{Matrix, RowReduction}, ring_field::Ring
};
use itertools::Itertools;
use std::{collections::HashMap, marker::PhantomData, ops::Mul};

pub struct AugmentedMatrix<TEntry: Ring, ML: Matrix<TEntry>, MR: Matrix<TEntry>> {
    pub left_matrix: ML,
    pub right_matrix: MR,
    pd: PhantomData<TEntry>,
}

impl<TEntry: Ring, ML: Matrix<TEntry>, MR: Matrix<TEntry>> AugmentedMatrix<TEntry, ML, MR> where ML::Transpose: Mul<MR>
{
    pub fn new(left_matrix: ML, right_matrix: MR) -> Self {
        assert_eq!(left_matrix.size().0, right_matrix.size().0, "wrong sizes");
        Self {
            left_matrix,
            right_matrix,
            pd: PhantomData,
        }
    }

    pub fn solve(mut self) -> Option<Self> {
        if let Ok((left_red, ops)) = self.left_matrix.try_reduce_to_rref() {
            self.right_matrix.apply_ops(ops);
            self.left_matrix = left_red;
            Some(self)
        } else {
            None
        }
    }

    pub fn gen_parametric_form(
        &self,
        l_names: Vec<String>,
        r_names: Vec<String>,
    ) -> Option<Vec<Function<TEntry>>> {
        if !self.left_matrix.is_rref() {
            //println!("Not rref");
            return None;
        }
        let pivots = self.left_matrix.pivots();
        let equations = (0..self.left_matrix.size().0)
            .map(|row| {
                let lhs = map_row_to_function(self.left_matrix.row(row), l_names.clone());
                let rhs = map_row_to_function(self.right_matrix.row(row), r_names.clone());
                Equation::new(lhs, rhs)
            })
            .collect_vec();

        let mut arr = (0..self.left_matrix.size().0)
            .map(|i| Function::Variable(l_names[i].clone()))
            .collect_vec();
        let mut map = HashMap::new();
        for (row, eq) in equations.into_iter().enumerate().rev() {
            if let Some(entry) = pivots.iter().find(|pos| pos.row == row).map(|pos| pos.col) {
                if let Some(func) = eq.solve_for(&l_names[entry]) {
                    let res = func.eval(&map);
                    if let Function::Undefined = res {
                        //println!("Couldn't eval {func:?} w/ {map:?}");
                        return None;
                    } else {
                        arr[entry] = res.clone();
                        map.insert(l_names[entry].clone(), res);
                    }
                } else {
                    //println!("Couldn't solve {eq:?} for {:?}", l_names[entry]);
                    return None;
                }
            } else {
                if !eq.equals_zero() {
                    // Inconsistent matrix (i.e. 0 0 0 0 0 | n, n!=0)
                    //println!("Inconsistent matrix");
                    return None;
                }
            }
        }

        Some(arr)
    }

    pub fn consistent(&self) -> Option<bool> {
        if !self.left_matrix.is_rref() {
            return None;
        }
        for row in 0..self.left_matrix.size().0 {
            if self
                .left_matrix
                .row(row)
                .find(|v| v != &&TEntry::zero())
                .is_none()
                && self
                    .right_matrix
                    .row(row)
                    .find(|v| v != &&TEntry::zero())
                    .is_some()
            {
                return Some(false);
            }
        }
        Some(true)
    }
}

fn map_row_to_function<'a, TEntry: Ring>(
    row: impl Iterator<Item = &'a TEntry>,
    names: Vec<String>,
) -> Function<TEntry> {
    row.enumerate()
        .map(|(col, v)| {
            if *v == TEntry::zero() {
                // the additive identity is a multiplicative absorbing element
                Function::Constant(v.clone())
            } else {
                Function::Product(
                    Box::new(Function::Constant(v.clone())),
                    Box::new(Function::Variable(names[col].clone())),
                )
            }
        })
        .reduce(|acc, v| {
            if let Function::Constant(_) = v {
                acc
            } else {
                Function::Sum(Box::new(acc), Box::new(v))
            }
        })
        .unwrap_or(Function::Constant(TEntry::zero()))
}

impl<TEntry: Ring, ML: Matrix<TEntry>, MR: Matrix<TEntry>> std::fmt::Debug
    for AugmentedMatrix<TEntry, ML, MR>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn rem_first_last(s: String) -> String {
            let mut c = s.chars();
            c.next();
            c.next_back();
            c.as_str().to_string()
        }
        let res: [Vec<_>; 2] = [
            self.left_matrix
                .lines()
                .into_iter()
                .map(rem_first_last)
                .collect(),
            self.right_matrix
                .lines()
                .into_iter()
                .map(rem_first_last)
                .collect(),
        ];
        let lines = res[0].len();
        for (l, (r0, r1)) in res[0].iter().zip(&res[1]).enumerate() {
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
            write!(f, "{}│{}", r0, r1)?;
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

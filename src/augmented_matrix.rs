use std::{array, collections::HashMap};

use crate::{
    function::{Equation, Function},
    matrix::Matrix,
    ring_field::Ring,
};

pub struct AugmentedMatrix<TEntry: Ring, const R: usize, const CL: usize, const CR: usize> {
    pub left_matrix: Matrix<TEntry, R, CL>,
    pub right_matrix: Matrix<TEntry, R, CR>,
}

impl<TEntry: Ring, const R: usize, const CL: usize, const CR: usize>
    AugmentedMatrix<TEntry, R, CL, CR>
{
    pub fn new(left_matrix: Matrix<TEntry, R, CL>, right_matrix: Matrix<TEntry, R, CR>) -> Self {
        Self {
            left_matrix,
            right_matrix,
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
        l_names: [String; CL],
        r_names: [String; CR],
    ) -> Option<[Function<TEntry>; CL]> {
        if !self.left_matrix.is_rref() {
            //println!("Not rref");
            return None;
        }
        let leading_entries = self.left_matrix.leading_entries();
        let equations: [Equation<TEntry>; R] = array::from_fn(|row| {
            let lhs = map_row_to_function(self.left_matrix.entries[row], l_names.clone());
            let rhs = map_row_to_function(self.right_matrix.entries[row], r_names.clone());
            Equation::new(lhs, rhs)
        });

        let mut arr = array::from_fn(|i| Function::Variable(l_names[i].clone()));
        let mut map = HashMap::new();
        for (eq, entry) in equations.into_iter().zip(leading_entries).rev() {
            if let Some(entry) = entry {
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
}

fn map_row_to_function<TEntry: Ring, const L: usize>(
    row: [TEntry; L],
    names: [String; L],
) -> Function<TEntry> {
    row.iter()
        .enumerate()
        .map(|(col, v)| {
            if *v == TEntry::additive_ident() {
                // the additive identity is a multiplicative absorbing element
                Function::Constant(*v)
            } else {
                Function::Product(
                    Box::new(Function::Constant(*v)),
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
        .unwrap_or(Function::Constant(TEntry::additive_ident()))
}

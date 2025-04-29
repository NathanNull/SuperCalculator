use super::*;
use crate::expression::polynomial::{Polynomial, PolynomialSolvable};

impl<TEntry: PolynomialSolvable, const N: usize> SquareMatrix<TEntry, N>
where
    [(); N + 1]:,
{
    pub fn eigenvalues(&self) -> Result<Vec<(TEntry, usize)>, ()> {
        let mut expr_form = self.clone().cast_into::<Polynomial<TEntry, { N + 1 }>>();
        expr_form = expr_form - SquareMatrix::ident() * Polynomial::new(vec![(TEntry::from(1), 1)]);
        let det = expr_form.determinant();
        let ungrouped_eigenvals = det.zeros()?;
        let mut eigenvals = vec![];
        for v in ungrouped_eigenvals {
            let mut found = false;
            for (existing_v, n) in eigenvals.iter_mut() {
                if v == *existing_v {
                    *n += 1;
                    found = true;
                    break;
                }
            }
            if !found {
                eigenvals.push((v, 1));
            }
        }
        Ok(eigenvals)
    }

    pub fn eigenspace(&self, eigenvalue: TEntry) -> Basis<TEntry, N, ColumnVector<TEntry, N>> {
        let test_matrix = self.clone() - SquareMatrix::ident() * eigenvalue;
        test_matrix.nullspace()
    }

    pub fn generalized_eigenspace(
        &self,
        eigenvalue: TEntry,
        rank: usize,
    ) -> Basis<TEntry, N, ColumnVector<TEntry, N>> {
        let mut test_matrix = self.clone() - SquareMatrix::ident() * eigenvalue;
        let original = test_matrix.clone();
        for _ in 1..rank {
            test_matrix = test_matrix * original.clone();
        }
        test_matrix.nullspace()
    }

    pub fn try_diagonalize(&self) -> Option<(Self, Self)> {
        if let Ok(eigenvalues) = self.eigenvalues() {
            let mut vals: [TEntry; N] = array::from_fn(|_| TEntry::from(0));
            let mut vecs = vals.each_ref().map(|_| ColumnVector::zero());
            let mut num_found = 0;
            for (eigenvalue, mult) in eigenvalues {
                let eigenspace = self.eigenspace(eigenvalue.clone());
                if eigenspace.dimension() < mult {
                    // Too few eigenvectors to construct our square matrix
                    return None;
                }
                for vec in eigenspace.vectors() {
                    vecs[num_found] = vec.clone();
                    vals[num_found] = eigenvalue.clone();
                    num_found += 1;
                }
            }
            if num_found != N {
                // If there are too few eigenvalues (and somehow the eigenvalues function doesn't catch that)
                // then we won't have the full array filled by the end of this process, so there is no
                // diagonalization possible.
                return None;
            }
            let p = Matrix::new_columns(vecs.map(|v| v.as_array()));
            let mut d = SquareMatrix::ident();
            for (idx, val) in vals.iter().enumerate() {
                d.entries[idx][idx] = val.clone();
            }
            Some((p, d))
        } else {
            // Couldn't find rational eigenvalues so can't diagonalize while remaining in the rationals
            None
        }
    }
}

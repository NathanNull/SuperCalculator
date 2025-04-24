use super::*;
use crate::expression::polynomial::{Polynomial, Term};
use crate::num::rational::Rational;
use crate::r;

impl<const N: usize> SquareMatrix<Rational, N> {
    pub fn eigenvalues(&self) -> Result<Vec<(Rational, usize)>, ()> {
        let mut expr_form = self.cast_into::<Polynomial<Rational>>();
        expr_form = expr_form
            - SquareMatrix::ident()
                * Polynomial::new(vec![Term::new(r!(1), vec![("λ".to_string(), 1)])]);
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

    pub fn eigenspace(
        &self,
        eigenvalue: Rational,
    ) -> Basis<Rational, N, ColumnVector<Rational, N>> {
        let test_matrix = self.clone() - SquareMatrix::ident() * eigenvalue;
        test_matrix.nullspace()
    }

    pub fn generalized_eigenspace(
        &self,
        eigenvalue: Rational,
        rank: usize,
    ) -> Basis<Rational, N, ColumnVector<Rational, N>> {
        let mut test_matrix = self.clone() - SquareMatrix::ident() * eigenvalue;
        let original = test_matrix.clone();
        for _ in 1..rank {
            test_matrix = test_matrix * original;
        }
        test_matrix.nullspace()
    }

    pub fn try_diagonalize(&self) -> Option<(Self, Self)> {
        if let Ok(eigenvalues) = self.eigenvalues() {
            let mut vals = [r!(0); N];
            let mut vecs = vals.each_ref().map(|_| ColumnVector::zero());
            let mut num_found = 0;
            for (eigenvalue, mult) in eigenvalues {
                let eigenspace = self.eigenspace(eigenvalue);
                if eigenspace.dimension() < mult {
                    // Too few eigenvectors to construct our square matrix
                    return None;
                }
                for vec in eigenspace.vectors() {
                    vecs[num_found] = vec.clone();
                    vals[num_found] = eigenvalue;
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

use crate::{
    matrix::{ColumnVector, Matrix, SizedMatrix, SquareMatrix},
    num::rational::Rational,
    ring_field::{Ring, TrueDiv},
};

pub struct SimpleMarkovChain<const STATES: usize> {
    transition_matrix: SquareMatrix<Rational, STATES>,
}

impl<const STATES: usize> SimpleMarkovChain<STATES> {
    pub fn new(transitions: [[Rational; STATES]; STATES]) -> Self {
        for r in &transitions {
            assert_eq!(
                (*r).into_iter().reduce(|a, b| a + b),
                Some(Rational::new(true, 1, 1))
            );
        }
        Self {
            transition_matrix: SquareMatrix::new(transitions).transpose(),
        }
    }

    pub fn step_probabilities(
        &self,
        current_state: ColumnVector<Rational, STATES>,
    ) -> ColumnVector<Rational, STATES> {
        self.transition_matrix.clone() * current_state
    }

    pub fn steady_state(&self) -> Option<ColumnVector<Rational, STATES>> {
        let eigenmatrix = self.transition_matrix.clone() - SquareMatrix::ident();
        println!("{:?}", eigenmatrix);
        if let Some(scaled_state) = eigenmatrix.nullspace().vectors().first() {
            let sum = scaled_state
                .rows()
                .fold(Rational::zero(), |acc, mut r| acc + *r.next().unwrap());
            SizedMatrix::from_unsized(scaled_state.clone() * sum.inverse())
        } else {
            None
        }

        //Ax=x
        //(A-I)x=0
    }
}

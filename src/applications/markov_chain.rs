use crate::{
    matrix::{ColumnVector, SquareMatrix},
    num::rational::Rational,
    r,
    vector_space::subspace::Basis,
};

pub struct SimpleMarkovChain<const STATES: usize> {
    transition_matrix: SquareMatrix<Rational, STATES>,
}

impl<const STATES: usize> SimpleMarkovChain<STATES> {
    pub fn new(transitions: [[Rational; STATES]; STATES]) -> Self {
        for r in &transitions {
            assert_eq!(
                r.clone().into_iter().reduce(|a, b| a + b),
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
        self.transition_matrix * current_state
    }

    pub fn steady_state(&self) -> Option<ColumnVector<Rational, STATES>> {
        let eigenmatrix = self.transition_matrix - SquareMatrix::ident();
        println!("{:?}", eigenmatrix);
        if let Some(scaled_state) = eigenmatrix.nullspace().vectors().first() {
            let sum = scaled_state.entries.iter().fold(r!(0), |acc, r| acc + r[0]);
            Some(*scaled_state * (r!(1) / sum))
        } else {
            None
        }

        //Ax=x
        //(A-I)x=0
    }
}

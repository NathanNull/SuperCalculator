use std::array;

use crate::{
    augmented_matrix::AugmentedMatrix,
    expression::polynomial::Polynomial,
    matrix::{ColumnVector, Matrix},
    num::rational::Rational,
    r,
};

#[macro_export]
macro_rules! data {
    ( $( ( $x_num:literal $(/ $x_den:literal)? , $y_num:literal $(/ $y_den:literal)? ) ),* $(,)? ) => {
        [$((r!($x_num $(/ $x_den)?),r!($y_num $(/ $y_den)?))),*]
    };
}

pub fn line_of_best_fit<const DATA: usize>(
    data: [(Rational, Rational); DATA],
) -> Polynomial<Rational, 2> {
    let a: Matrix<Rational, DATA, 2> = Matrix::new(array::from_fn(|i| [r!(1), data[i].0]));
    let a_t = a.transpose();
    let b = ColumnVector::v_new(data.each_ref().map(|point| point.1));

    let lhs = a_t * a;
    let rhs = a_t * b;
    let soln = AugmentedMatrix::new(lhs, rhs)
        .solve()
        .unwrap()
        .right_matrix
        .as_array();

    Polynomial::new(vec![(soln[1], 1), (soln[0], 0)])
}

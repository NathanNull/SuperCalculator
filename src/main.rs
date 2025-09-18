#![feature(generic_const_exprs, iter_array_chunks)]
#![allow(incomplete_features)]

#[allow(unused_imports)]
use crate::{
    augmented_matrix::AugmentedMatrix,
    expression::{
        function::Function,
        polynomial::{Polynomial, UnsizedPolynomial},
    },
    matrix::Matrix,
    num::constructible::Constructible,
    num::{rational::Rational, real::Real},
    ring_field::Sqrt,
    ring_field::TrueDiv as _,
    vector_space::Cross as _,
    vector_space::Vector as _,
};
use examples::Examples;

mod applications;
mod augmented_matrix;
mod debug_multi;
mod examples;
mod expression;
mod if_trait;
mod matrix;
mod num;
mod ring_field;
mod vector_space;

fn main() -> Result<(), &'static str> {
    let c2: Constructible = r!(2).into();
    let sqrt2 = c2.sqrt();

    let c3: Constructible = r!(3).into();
    let sqrt3 = c3.sqrt();

    let sum = sqrt2.clone() * sqrt2 / (sqrt3.clone() * sqrt3);

    let res = sum.test_approx(sum.approx());
    println!("res: {:?} from {sum:?} using {:?}", res, sum.approx());

    Ok(())
}

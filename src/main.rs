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

#[allow(unused)]
use examples::Examples;

mod applications;
mod augmented_matrix;
mod debug_multi;
mod examples;
mod expression;
mod if_trait;
mod matrix;
mod num;
mod repl;
mod ring_field;
mod vector_space;

fn main() -> Result<(), &'static str> {
    let v = fmatrix!(3;4;0);
    println!("Vector magnitude: {:?}", v.magnitude());

    Ok(())
}

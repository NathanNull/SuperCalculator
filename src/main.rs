#![feature(generic_const_exprs, iter_array_chunks, specialization)]
#![allow(incomplete_features)]

use crate::num::complex::Complex;
#[allow(unused)]
use crate::{
    augmented_matrix::AugmentedMatrix,
    expression::{
        function::Function,
        polynomial::{Polynomial, UnsizedPolynomial},
    },
    matrix::Matrix,
    num::constructible::Constructible,
    num::{rational::Rational, real::Real},
    repl::Repl,
    ring_field::Sqrt,
    ring_field::TrueDiv as _,
    vector_space::Cross as _,
    vector_space::Vector as _,
};
#[allow(unused)]
use examples::Examples;
use std::error::Error;

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

fn main() -> Result<(), Box<dyn Error>> {
    Repl::run_repl()?;
    Ok(())
}

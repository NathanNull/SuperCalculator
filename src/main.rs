#![feature(generic_const_exprs)]

use matrix::Matrix;
use rational::Rational;
use vector_space::span::Span;

mod augmented_matrix;
mod debug_multi;
mod function;
mod if_trait;
mod matrix;
mod rational;
mod ring_field;
mod vector_space;

/// Example: ```matrix!(1,2,3;4,5,6;7,8,9)```
macro_rules! matrix {
    ($($($v:expr),+);+) => {
        Matrix::new([$([$(r!($v)),*]),*])
    };
}

const VARS: &str = "abcedfghijklmnopqrstuvwxyz";

fn main() -> Result<(), &'static str> {
    let m = matrix!(1,2,3;4,5,6;7,8,9;0,0,0);
    let column_space = m.column_space();
    println!("{m:?} = M");
    println!("{:?}: Column space", column_space);
    println!("Rank(M) = {:?}", column_space.dimension());
    Ok(())
}

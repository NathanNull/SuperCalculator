#![feature(generic_const_exprs, iter_array_chunks, specialization)]
#![allow(incomplete_features)]

use crate::repl::Repl;
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

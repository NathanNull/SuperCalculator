use std::{array, collections::HashMap};

use augmented_matrix::AugmentedMatrix;
use function::{EvalResult, Function};
use matrix::Matrix;
use rational::Rational;

mod augmented_matrix;
mod function;
mod matrix;
mod rational;
mod ring_field;

macro_rules! matrix {
    ($($($v:expr),+);+) => {
        Matrix::new([$([$(r!($v)),+]),+])
    };
}

const VARS: &str = "abcedfghijklmnopqrstuvwxyz";

fn main() {
    let m = matrix!(1,2;2,4);
    let s = matrix!(1;3);
    let aug = AugmentedMatrix::new(m, s);
    if let Some(aug) = aug.solve() {
        println!("Matrix: \n{:?}\n{:?}", aug.left_matrix, aug.right_matrix);
        if let Some(funcs) = aug.gen_parametric_form(
            array::from_fn(|i| {
                VARS.chars()
                    .nth(i)
                    .map(|c| c.to_string())
                    .unwrap_or(format!("var{i}")) // If you can somehow manage to have more than 26 variables
            }),
            ["one".to_string()],
        ) {
            let funcs = funcs.map(|f| {
                if let EvalResult::Res(sub) = f.eval(&HashMap::from_iter([(
                    "one".to_string(),
                    Function::Constant(r!(1)),
                )])) {
                    sub
                } else {
                    f
                }
            });
            println!(
                "Functions are:\r\n{}",
                funcs.map(|f| format!("{f:?}")).join("\r\n")
            )
        } else {
            println!("Couldn't generate parametric form")
        }
    }
}

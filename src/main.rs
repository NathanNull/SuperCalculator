use std::{array, collections::HashMap};

use augmented_matrix::AugmentedMatrix;
use function::Function;
use matrix::{Matrix, Vector};
use rand::rng;
use rational::Rational;
use ring_field::Ring;

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

fn main() -> Result<(), &'static str> {
    let m = matrix!(1,2,3;4,5,6;7,8,9);
    let s = matrix!(1;3;5);

    let aug = AugmentedMatrix::new(m, s);
    let aug = aug.solve().ok_or("Failed")?;
    println!("Matrix: \n{:?}\n{:?}", aug.left_matrix, aug.right_matrix);

    let funcs = aug
        .gen_parametric_form(
            array::from_fn(|i| {
                VARS.chars()
                    .nth(i)
                    .map(|c| c.to_string())
                    .unwrap_or(format!("var{i}")) // If you can somehow manage to have more than 26 variables
            }),
            ["one".to_string()],
        )
        .ok_or("Couldn't generate parametric form")?;
    let funcs = funcs.map(|f| {
        f.eval(&HashMap::from_iter([(
            "one".to_string(),
            Function::Constant(r!(1)),
        )]))
    });

    println!(
        "Functions are:\r\n{}",
        funcs.each_ref().map(|f| format!("{f:?}")).join("\r\n")
    );

    for _ in 0..20 {
        let mut vars = HashMap::new();
        for f in &funcs {
            for v in f.variables() {
                if !vars.contains_key(&v) {
                    vars.insert(v, Function::Constant(Rational::generate(&mut rng())));
                }
            }
        }
        let test = Vector::v_new(funcs.each_ref().map(|f| match f.eval(&vars) {
            Function::Constant(v) => v,
            _ => panic!("failed to evaluate"),
        }));
        println!("Test value:\n{test:?}");
        assert_eq!(m * test, s);
    }
    Ok(())
}

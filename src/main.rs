#![feature(generic_const_exprs, iter_array_chunks)]
#![allow(incomplete_features)]

use std::{array, collections::HashMap};

use applications::hill_cipher::{decode, try_break_code};
use expression::polynomial::{Polynomial, Term};
use matrix::{ColumnVector, Matrix};
use num::cyclic_group::ZMod;
use num::rational::Rational;
use ring_field::{Real, Ring};
use vector_space::subspace::Subspace;

mod applications;
mod augmented_matrix;
mod debug_multi;
mod expression;
mod if_trait;
mod matrix;
mod num;
mod ring_field;
mod vector_space;

/// Example: ```matrix!(1,2,3;4,5,6;7,8,9)```
#[macro_export]
macro_rules! matrix {
    ($( $( $num:literal $(/$den:literal)? ),+ );+ ) => {
        Matrix::new([ $( [ $( {
            r!($num $(/$den)?)
        } ),* ] ),* ])
    };
}

#[macro_export]
macro_rules! fmatrix {
    ($( $( $num:literal ),+ );+ ) => {
        Matrix::new([ $( [ $( {
            Real($num as f64)
        } ),* ] ),* ])
    };
}

#[macro_export]
macro_rules! zmatrix {
    (<$n: literal> $( $( $num:literal ),+ );+) => {
        Matrix::new([ $( [ $( {
            ZMod::<$n>::new($num as usize)
        } ),* ] ),* ])
    };
}

fn main() -> Result<(), &'static str> {
    // try_break_code::<3>(
    //     &"ZSALFRLKOVDFWAUBCO".to_lowercase(),
    //     &"CRYPTOGRAPHYISCOOL".to_lowercase(),
    // );
    // decode(&"NTWEVJFCMTYIMHQZ".to_lowercase(), zmatrix!(<26>6,3;9,8));
    let x = "x".to_string();
    //x^4 + 10 x^3 + 35 x^2 + 50 x + 24
    let p = Polynomial::new(vec![
        Term::new(1.into(), vec![(x.clone(), 4)]),
        Term::new(10.into(), vec![(x.clone(), 3)]),
        Term::new(35.into(), vec![(x.clone(), 2)]),
        Term::new(50.into(), vec![(x.clone(), 1)]),
        Term::new(24.into(), vec![]),
    ]);
    println!("Zeros of {p:?} are {:?}", p.zeros());
    Ok(())
    // let m = matrix!(1,0,0;0,1,0;0,0,0);
    // let s = Subspace::new(m.columns());
    // println!("{}", s.contains(matrix!(1;0;1)));

    // let a = matrix!(-3,3,6;-4,5,4;-4,2,7);
    // let e_val = r!(3);

    // let a_li = a - SquareMatrix::ident() * e_val;
    // let a_li2 = a_li * a_li;
    // println!("{:?}", AugmentedMatrix::new(a_li, a_li2));

    // let nul = a_li.nullspace();
    // println!("{nul:?} = nullspace");

    // let nul = a_li2.nullspace();
    // println!("{nul:?} = nullspace");

    // println!(
    //     "{:?}",
    //     Span::new([
    //         matrix!(1;0;0),
    //         a_li * matrix!(1;0;0),
    //         a_li2 * matrix!(1;0;0)
    //     ])
    // );

    // let m = matrix!(-6,2;1,-1);
    // let nm2 = m * m * r!(-1);

    // let m_col = m.to_column();
    // let i_col = matrix!(1;0;0;1);
    // let aug = AugmentedMatrix::new(
    //     Matrix::new_columns([m_col.as_array(), i_col.as_array()]),
    //     nm2.to_column(),
    // )
    // .solve()
    // .unwrap();

    // let pf = aug.gen_parametric_form(
    //     array::from_fn(|i| VARS[i..=i].to_string()),
    //     ["1".to_string()],
    // ).unwrap();

    // println!("{pf:?}");

    // let vec = ColumnVector::v_new(
    //     pf.map(|f| {
    //         f.eval(&HashMap::from_iter(
    //             VARS.chars()
    //                 .map(|c| (c.to_string(), Function::Constant(r!(1)))),
    //         ))
    //     })
    //     .map(|f| {
    //         if let Function::Constant(v) = f {
    //             v
    //         } else {
    //             panic!("Eval failed")
    //         }
    //     }),
    // );

    // println!("{vec:?}: lc");

    // Finding eigenvectors from eigenvalues
    // let m = matrix!(2,5,5;-4,-7,-4;-1,-1,-4);
    // let e = r!(-3);

    // let new_m = m.clone() - Matrix::ident() * e;
    // //println!("{new_m:?}");
    // let null = AugmentedMatrix::new(new_m, ColumnVector::zero())
    //     .solve()
    //     .unwrap();
    // println!("{null:?} from eigenvalue {e:?}");
    // let pf = null.gen_parametric_form(
    //     array::from_fn(|i| VARS[i..=i].to_string()),
    //     ["1".to_string()],
    // );
    // println!("Parametric form: {pf:?}");
    // let arr = pf.unwrap();
    // let vec = ColumnVector::v_new(
    //     arr.map(|f| {
    //         f.eval(&HashMap::from_iter(
    //             VARS.chars()
    //                 .map(|c| (c.to_string(), Function::Constant(r!(1)))),
    //         ))
    //     })
    //     .map(|f| {
    //         if let Function::Constant(v) = f {
    //             v
    //         } else {
    //             panic!("Eval failed")
    //         }
    //     }),
    // );
    // let square_scale = vec
    //     .entries
    //     .map(|r| r[0] * r[0])
    //     .into_iter()
    //     .reduce(|acc, r| acc + r)
    //     .unwrap()
    //     .inverse();
    // println!("{:?} * sqrt({:?}): Unit eigenvector", vec, square_scale);

    // Finding generalized eigenvectors (goes with previous)
    // let gen_aug = AugmentedMatrix::new(new_m*new_m, ColumnVector::zero());
    // println!("{gen_aug:?}");
    // let gen_aug = gen_aug.solve().unwrap();
    // println!("{gen_aug:?}");
    // let gen = gen_aug.gen_parametric_form(
    //     array::from_fn(|i| VARS[i..=i].to_string()),
    //     ["1".to_string()],
    // );

    // let garr = gen.unwrap();
    // let gvec = ColumnVector::v_new(
    //     garr.map(|f| {
    //         f.eval(&HashMap::from_iter(
    //             VARS.chars()
    //                 .map(|c| (c.to_string(), Function::Constant(r!(1)))),
    //         ))
    //     })
    //     .map(|f| {
    //         if let Function::Constant(v) = f {
    //             v
    //         } else {
    //             panic!("Eval failed")
    //         }
    //     }),
    // );

    // println!("{gvec:?}: Generalized eigenvector");

    // println!("{:?}: Initial eigenvector", new_m*gvec);

    // println!("{:?}: Should be 0,0,0", new_m*new_m*gvec);

    // let m = matrix!(0,-5,0;0,-5,0;-4,-5,4);
    // let v = matrix!(6;8;1);
    // let test_e = (m * v).entries[2][0] / v.entries[2][0];
    // if v * test_e == m * v {
    //     println!("Yes")
    // } else {
    //     println!("No")
    // }
}

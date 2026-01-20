use std::{array, sync::LazyLock};

use super::{value::ValueClone, *};
use crate::{
    matrix::Matrix,
    num::{complex::Complex, rational::Rational, real::Real},
};
use parce::parser;
use parce::parser::{
    recursive::p_recursive,
    stringbased::{p_alnum, p_n, p_regex},
};
use seq_macro::seq;

type LockedParser<Out> = LazyLock<Arc<dyn Parser<str, Out = Out>>>;

static WHITESPACE: LockedParser<()> = LazyLock::new(|| p_regex(r"\s*").ignore().box_clone());

static P_VAL: LockedParser<(Box<dyn Value>,)> = LazyLock::new(|| {
    let p_integer = p_n::<i32>();
    let p_rational = parser!(('-'?) & (p_n::<u64>()) & (!'/') & (p_n::<u64>()))
        .map(|(neg, num, den)| Rational::new(neg.is_none(), num, den));
    let p_real = parser!(('-'?) & (p_n::<u64>()) & (!'.') & (p_n::<u64>())).map(|(sign, int, dec)| {
        Real(
            (int as f64
                + str::parse::<f64>(&("0.".to_string() + &dec.to_string()))
                    .expect("Valid float literal")) * if sign.is_some() {-1.} else {1.},
        )
    });

    let p_r = parser!(
        (p_real.box_clone())
            | (p_rational
                .box_clone()
                .map(|r| Real(r.num() as f64 / r.den() as f64)))
            | (p_integer.box_clone().map(|r| Real(r as f64)))
    );

    let p_complex_inner = parser!(
        // Pure imaginary, ri
        (((p_r.box_clone().or(().map(|_| Real(1.)))) & (!'i'))
            >> |imag| Complex {
                real: Real(0.),
                imag
            })
            // Real maybe +/- imaginary, a (+/-) bi or just a
            | (((p_r.box_clone())
                & (WHITESPACE.clone())
                & ((('+' | '-')
                    & (WHITESPACE.clone())
                    & (p_r.box_clone().or(().map(|_| Real(1.))))
                    & (!'i'))
                    | (().map_raw(|_| Some(('+', Real(0.)))))))
                >> |(real, sign, imag)| Complex {
                    real,
                    imag: if sign == '+' { imag } else { -imag }
                })
    );
    let p_complex =
        parser!((!"c[") & (WHITESPACE.clone()) & p_complex_inner & (WHITESPACE.clone()) & (!']'));

    let p_num = parser!(
        (p_real.map(|r| r.box_clone()))
            | (p_rational.map(|r| r.box_clone()))
            | (p_integer.map(|r| r.box_clone()))
            | (p_complex.map(|r| r.box_clone()))
    );
    let p_array = parser![
        (!'['),
        (WHITESPACE.box_clone()),
        (p_num.box_clone().sep_by(
            parser!((WHITESPACE.clone()) & ',' & (WHITESPACE.clone())),
            1..
        )),
        (WHITESPACE.box_clone()),
        (!']')
    ];
    let p_arr_many = p_array.box_clone().sep_by(
        parser!((WHITESPACE.clone()) & ',' & (WHITESPACE.clone())),
        1..,
    );
    let p_mat_inner =
        parser!((!'[') & (WHITESPACE.clone()) & (p_arr_many) & (WHITESPACE.clone()) & (!']'));
    let p_mat = p_mat_inner.try_map(|vals: Vec<Vec<Box<dyn Value>>>|{
        let rows = vals.len();
        let cols = vals.first().map(|r|r.len()).unwrap_or_default();
        let mut entry = None;
        for row in &vals {
            if row.len() != cols {
                return None;
            }
            for val in row {
                let ty = val.get_type();
                if entry.is_some_and(|e|e != ty) {
                    return None;
                }
                entry = Some(ty);
            }
        }

        // This is awful
        // I really need an owned unsized matrix type
        // TODO: do that
        seq!(R in 1..=3 {
            seq!(C in 1..=3 {
                if rows == R && cols == C {
                    return match entry {
                        Some(ValueType::Integer) | None => {
                            Some(Matrix::<i32, R, C>::new(array::from_fn(|r|array::from_fn(|c|*vals[r][c].downcast::<i32>().unwrap()))).box_clone())
                        }
                        Some(ValueType::Real) => {
                            Some(Matrix::<Real, R, C>::new(array::from_fn(|r|array::from_fn(|c|*vals[r][c].downcast::<Real>().unwrap()))).box_clone())
                        }
                        Some(ValueType::Rational) => {
                            Some(Matrix::<Rational, R, C>::new(array::from_fn(|r|array::from_fn(|c|*vals[r][c].downcast::<Rational>().unwrap()))).box_clone())
                        }
                        _ => {
                            None
                        }
                    }
                }
            });
        });

        None
    });

    parser!(p_mat | p_num).box_clone()
});

static P_EXPR: LockedParser<(Expression,)> = LazyLock::new(|| {
    let p_op = [
        ("+", Op::Add),
        ("-", Op::Sub),
        ("*", Op::Mul),
        ("/", Op::Div),
        (".", Op::Dot),
        ("x", Op::Cross),
    ]
    .into_iter()
    .map(|(pat, op)| pat.map(move |_| op).box_clone())
    .reduce(|a, b| a.or(b).box_clone())
    .expect("There is at least one operation");
    p_recursive::<str, (Expression,), _>(|p| {
        let p_lhs =
            parser!((P_VAL.clone().map(Expression::Value)) | (p_alnum().map(Expression::Variable)));
        let rhs = WHITESPACE
            .clone()
            .and(p_op)
            .and(WHITESPACE.clone())
            .and(p.clone());

        let builder = |(lhs, rhs)| match rhs {
            Some((op, rhs)) => Expression::Binop(Box::new(lhs), op, Box::new(rhs)),
            None => lhs,
        };

        let lhs = parser!(p_lhs | ((!'(') & p & (!')')));

        parser!((lhs & rhs?) >> builder)
    })
    .box_clone()
});

pub static PARSER: LockedParser<(Statement,)> = LazyLock::new(|| {
    // Assignments
    let p_assign = parser!(
        (p_alnum()) & (WHITESPACE.clone()) & (!"=") & (WHITESPACE.clone()) & (P_EXPR.clone())
    );

    // Commands
    let p_command = parser!((!'%') & (WHITESPACE.clone()) & (p_alnum()));

    // Statements
    let p_statement = parser!(
        (p_assign.map(|(name, val)| Statement::Assign(name, val)))
            | (P_EXPR.clone().map(Statement::Expression))
            | (p_command.map(Statement::Command))
    );

    parser!((WHITESPACE.clone()) & p_statement & (WHITESPACE.clone())).box_clone()
});

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Dot,
    Cross,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Value(Box<dyn Value>),
    Variable(Arc<str>),
    Binop(Box<Expression>, Op, Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    Assign(Arc<str>, Expression),
    Command(Arc<str>),
}

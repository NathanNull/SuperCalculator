use crate::{
    matrix::Matrix, num::{rational::Rational, real::Real}, p, repl::value::ValueClone
};
use parser::*;
use seq_macro::seq;
use std::{
    collections::HashMap,
    error::Error,
    fmt::Debug,
    io::Write,
    sync::{Arc, LazyLock},
    array,
    ops::BitOr,
};

mod parser;
mod range;
mod tuple;
mod value;

pub use value::{Downcast, Value, ValueType};

static PARSER: LazyLock<Parser<Statement>> = LazyLock::new(|| {
    let p_integer = p_n::<i32>();
    let p_rational = (p!('-').maybe() & p_n::<u64>() & !p!('/') & p_n::<u64>())
        .map(|(neg, num, den)| Rational::new(neg.is_none(), num, den));
    let p_real = (p_n::<i64>() & !p!('.') & p_n::<u64>()).map(|(int, dec)| {
        Real(
            int as f64
                + ("0.".to_string() + &dec.to_string())
                    .parse::<f64>()
                    .expect("Valid float literal"),
        )
    });
    let p_whitespace = !p_regex(r"\s*");

    // Values
    let p_num = p_real.map(|r| r.into_box())
        | p_rational.map(|r| r.into_box())
        | p_integer.map(|r| r.into_box());
    let p_array = !p!('[')
        & p_whitespace.clone()
        & p_num
            .clone()
            .sep_by(p_whitespace.clone() & !p!(',') & p_whitespace.clone(), 1..)
        & p_whitespace.clone()
        & !p!(']');
    let p_mat_inner = !p!('[')
        & p_whitespace.clone()
        & p_array
            .clone()
            .sep_by(p_whitespace.clone() & !p!(',') & p_whitespace.clone(), 1..)
        & p_whitespace.clone()
        & !p!(']');
    let p_mat = p_mat_inner.try_map(|vals|{
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
        seq!(R in 0..=5 {
            seq!(C in 0..=5 {
                if rows == R && cols == C {
                    return match entry {
                        Some(ValueType::Integer) | None => {
                            println!("{},{},i32", R, C);
                            Some(Matrix::<i32, R, C>::new(array::from_fn(|r|array::from_fn(|c|*vals[r][c].downcast::<i32>().unwrap()))).box_clone())
                        }
                        Some(ValueType::Real) => {
                            println!("{},{},real", R, C);
                            Some(Matrix::<Real, R, C>::new(array::from_fn(|r|array::from_fn(|c|*vals[r][c].downcast::<Real>().unwrap()))).box_clone())
                        }
                        Some(ValueType::Rational) => {
                            println!("{},{},rational", R, C);
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

    let p_value = p_mat | p_num;

    // Expressions
    let p_op = [("+", Op::Add), ("-", Op::Sub), ("*", Op::Mul), ("/", Op::Div), (".", Op::Dot), ("x", Op::Cross)]
        .into_iter()
        .map(|(pat, op)| p!(pat).map(move |_| op))
        .reduce(<_ as BitOr>::bitor)
        .expect("There is at least one operation");
    let p_expression = p_recursive(|p| {
        let p_lhs =
            p_value.clone().map(Expression::Value)
            | p_alnum().map(Expression::Variable);
        (p_lhs & (p_whitespace.clone() & p_op & p_whitespace.clone() & p).maybe()).map(|(lhs, rhs)|match rhs {
            Some((op, rhs)) => Expression::Binop(Box::new(lhs), op, Box::new(rhs)),
            None => lhs
        })
    });

    // Assignments
    let p_assign =
        p_alnum() & p_whitespace.clone() & !p!("=") & p_whitespace.clone() & p_expression.clone();

    // Commands
    let p_command = !p!('%') & p_whitespace.clone() & p_alnum();

    // Statements
    let p_statement = p_assign.map(|(name, val)| Statement::Assign(name, val))
        | p_expression.map(Statement::Expression)
        | p_command.map(Statement::Command);

    p_whitespace.clone() & p_statement.clone() & p_whitespace.clone()
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
enum Expression {
    Value(Box<dyn Value>),
    Variable(Arc<str>),
    Binop(Box<Expression>, Op, Box<Expression>),
}

#[derive(Debug)]
enum Statement {
    Expression(Expression),
    Assign(Arc<str>, Expression),
    Command(Arc<str>),
}

pub struct Repl {
    variables: HashMap<Arc<str>, Box<dyn Value>>,
    done: bool,
}

#[allow(unused)]
impl Repl {
    pub fn run_repl() -> Result<(), std::io::Error> {
        let a = (Box::new(1) as Box<dyn Value>).downcast::<i32>().expect("should work");
        let mut repl = Self {
            variables: HashMap::new(),
            done: false,
        };
        let mut stdout = std::io::stdout();
        let mut stdin = std::io::stdin();
        while !repl.done {
            print!("> ");
            stdout.flush();
            let mut nextline = String::new();
            stdin.read_line(&mut nextline)?;
            match PARSER.parse_full(&nextline) {
                Ok(statement) => match repl.run_statement(&statement) {
                    Ok(res) => println!("{res}"),
                    Err(e) => println!("Runtime error: {e}"),
                },
                Err(e) => println!("Parse error: {e}"),
            }
        }
        Ok(())
    }

    fn run_statement(&mut self, statement: &Statement) -> Result<String, Box<dyn Error>> {
        let res = match statement {
            Statement::Expression(expression) => self.run_expression(expression)?,
            Statement::Assign(name, expr) => {
                let value = self.run_expression(expr)?;
                self.variables.insert(name.clone(), value.clone());
                value
            }
            Statement::Command(cmd) => {
                match &**cmd {
                    "exit" => self.done = true,
                    _ => return Err("Unrecognized command".into()),
                }
                Box::new(())
            }
        };
        Ok(format!("{res}"))
    }

    fn run_expression(&self, expression: &Expression) -> Result<Box<dyn Value>, Box<dyn Error>> {
        match expression {
            Expression::Value(value) => Ok(value.clone()),
            Expression::Variable(name) => self
                .variables
                .get(name)
                .cloned()
                .ok_or_else(|| "Unknown variable".into()),
            Expression::Binop(lhs, op, rhs) => {
                let lhs_val = self.run_expression(lhs)?;
                let rhs_val = self.run_expression(rhs)?;
                lhs_val.try_op(*op, rhs_val)
            }
        }
    }
}

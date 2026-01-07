use std::{collections::HashMap, error::Error, rc::Rc};

mod parser;
mod range;
mod tuple;

use parser::*;

pub enum ValueType {
    Integer,
    Rational,
    Real,
}

#[allow(unused)]
pub trait Value {
    fn get_type(&self) -> ValueType;
}

pub struct Repl {
    variables: HashMap<Rc<str>, Box<dyn Value>>,
}

#[allow(unused)]
impl Repl {
    fn run_line(&mut self, statement: String) -> Result<String, Box<dyn Error>> {
        Err("Not implemented".into())
    }
}
use parce::parser::Parser;
use rustyline::DefaultEditor;

use std::{collections::HashMap, error::Error, sync::Arc};

mod parser;
mod value;

pub use parser::Op;
use parser::{Expression, PARSER, Statement};
pub use value::{Downcast, Value, ValueType};

pub struct Repl {
    variables: HashMap<Arc<str>, Box<dyn Value>>,
    done: bool,
}

#[allow(unused)]
impl Repl {
    pub fn run_repl() -> Result<(), Box<dyn Error>> {
        let mut terminal = DefaultEditor::new()?;
        let mut repl = Self {
            variables: HashMap::new(),
            done: false,
        };
        while !repl.done {
            let nextline = terminal.readline("> ")?;
            match PARSER.parse_full(&nextline) {
                Ok(statement) => match repl.run_statement(&statement) {
                    Ok(res) => println!("{res}"),
                    Err(e) => println!("Runtime error: {e}"),
                },
                Err(e) => {
                    println!("Parse error: {e}");
                    println!("Partial parse: {:?}", PARSER.parse(&nextline));
                },
            }
            terminal.add_history_entry(nextline);
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

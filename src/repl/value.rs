use std::{any::Any, error::Error, fmt::Display};

use crate::{debug_multi::DebugMulti, repl::Op};

#[derive(Debug, PartialEq, Eq)]
pub enum ValueType {
    Integer,
    Rational,
    Real,
    Complex,
    Void,
    Matrix(usize, usize),
}

pub trait ValueClone<'a> {
    fn box_clone(&self) -> Box<dyn Value + 'a>;
}

impl<'a, T: Clone + Value + 'a> ValueClone<'a> for T {
    fn box_clone(&self) -> Box<dyn Value + 'a> {
        Box::new(self.clone())
    }
}

#[allow(unused)]
pub trait Value: DebugMulti + for<'a> ValueClone<'a> + Any + Send + Sync {
    fn get_type(&self) -> ValueType;
    fn try_op(&self, op: Op, rhs: Box<dyn Value>) -> Result<Box<dyn Value>, Box<dyn Error>>;
}

#[macro_export]
macro_rules! try_ops_trait {
    ($(($($acc:tt)*))? trait $trait:ident {
        fn $f:ident (&self $(, $arg:ident : $argtype:ty)*$(,)?) -> $ret:ty {
            $(if ([$($gen: tt)*]: Self = $self:ty, $($req:tt)*) {
                $yes_implement:expr
            } else)* $(([$($basegen: tt)*]: Self = $baseself:ty, $($basereq:tt)*))? {
                $no_implement:expr
            }
        }
    }) => {
        $($($acc)*)? trait $trait {
            fn $f(&self $(, $arg : $argtype)*) -> $ret;
        }
        mod $f {
            
            $crate::_tryops_default_impl!($trait, $f, $no_implement, ($($arg: $argtype),*), $ret, $(if ([$($basegen)*]: Self = $baseself, $($basereq)*))?);
            
            use super::*;
            macro_rules! trait_fun{
                ($body:expr) => {
                    fn $f(&self $(, $arg : $argtype)*) -> $ret {
                        ($body)(self, $($arg),*)
                    }
                };
            }
            $(impl<$($gen)*> $trait for $self where $($req)* {
                trait_fun!($yes_implement);
            })*
        }
    };
}

#[macro_export]
macro_rules! _tryops_default_impl {
    ($trait:ident, $f:ident, $no_implement:expr, ($($arg:ident : $argtype:ty),*), $ret:ty, ) => {
        impl<T> $trait for T {
            default fn $f(&self $(, $arg : $argtype)*) -> $ret {
                ($no_implement)(self, $($arg),*)
            }
        }
    };
    ($trait:ident, $f:ident, $no_implement:expr, ($($arg:ident : $argtype:ty),*), $ret:ty, if ([$($bg:tt)*]: Self = $bs:ty, $($br:tt)*)) => {
        impl<$($bg)*> $trait for $bs where $($br)* {
            default fn $f(&self $(, $arg : $argtype)*) -> $ret {
                ($no_implement)(self, $($arg),*)
            }
        }
    };
}

pub trait Downcast<'a>
where
    Self: 'a,
{
    fn downcast<T: Value>(&'a self) -> Option<&'a T>;
}

impl<'a> Downcast<'a> for Box<dyn Value> {
    fn downcast<T: Value>(&self) -> Option<&T> {
        (&**self as &dyn Any).downcast_ref()
    }
}

impl<'a> Downcast<'a> for &'a dyn Value {
    fn downcast<T: Value>(&'a self) -> Option<&'a T> {
        (*self as &'a dyn Any).downcast_ref()
    }
}

impl Value for () {
    fn get_type(&self) -> ValueType {
        ValueType::Void
    }
    fn try_op(&self, _op: Op, _rhs: Box<dyn Value>) -> Result<Box<dyn Value>, Box<dyn Error>> {
        Err("Void type has no operations".into())
    }
}

impl Clone for Box<dyn Value> {
    fn clone(&self) -> Self {
        self.box_clone()
    }
}

impl Display for Box<dyn Value> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {:?}", self.get_type(), self)
    }
}

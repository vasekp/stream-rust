use crate::base::*;

pub use std::rc::{Rc, Weak};
use std::collections::HashMap;

/// The environment in which expressions are evaluated (variable assignments made using `with`). 
/// This is passed as an argument to [`Expr::eval()`].
#[derive(Default, Clone)]
pub struct Env {
    pub vars: HashMap<String, Rhs>,
    pub alpha: Alphabet,
}

impl Env {
    fn is_trivial(&self) -> bool {
        self.vars.is_empty()
    }

    pub(crate) fn wrap_describe(&self, call: impl FnOnce(u32) -> String, prec: u32) -> String {
        match self.is_trivial() {
            true => call(prec),
            false => format!("with({}, {})", self.describe(), call(0))
        }
    }

    /// The alphabet used for ordering characters and arithmetic operations on them.
    pub fn alphabet(&self) -> &Alphabet { &self.alpha }

    fn describe(&self) -> String {
        let mut iter = self.vars.iter()
            .map(|(key, val)| match val {
                Rhs::Value(item) => format!("{}={}", key, item.describe_prec(1)),
                Rhs::Function(expr) => format!("{}={{{}}}", key, expr.describe_prec(0))
            });
        let mut ret = match iter.next() {
            Some(rec) => rec,
            None => return String::new()
        };
        for rec in iter {
            ret.push(',');
            ret += &rec;
        }
        ret
    }
}

#[derive(Clone)]
pub enum Rhs {
    Value(Item),
    Function(Expr)
}

use crate::base::*;

pub use std::rc::{Rc, Weak};
use std::collections::HashMap;

/// The environment in which expressions are evaluated (variable assignments made using `with`). 
/// This is passed as an argument to [`Expr::eval()`].
#[derive(Default, Clone)]
pub struct Env {
    pub vars: HashMap<String, Rhs>,
    pub alpha: Rc<Alphabet>,
}

impl Env {
    pub(crate) fn wrap_describe(self: &Rc<Self>, call: impl FnOnce(u32, &Rc<Env>) -> String, prec: u32, env_outer: &Rc<Env>) -> String {
        self.alpha.wrap_describe(|prec|
            match self.vars.is_empty() {
                true => call(prec, env_outer),
                false => format!("with({}, {})", self.describe(env_outer), call(0, env_outer))
            },
            prec)
    }

    /// The alphabet used for ordering characters and arithmetic operations on them.
    pub fn alphabet(&self) -> &Rc<Alphabet> { &self.alpha }

    fn describe(&self, env: &Rc<Env>) -> String {
        let mut iter = self.vars.iter()
            .map(|(key, val)| match val {
                Rhs::Value(item) => format!("{}={}", key, item.describe_inner(1, env)),
                Rhs::Function(expr) => format!("{}={{{}}}", key, expr.describe_inner(0, env))
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

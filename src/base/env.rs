use crate::base::*;
use super::tracing::Tracer;

pub use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::collections::HashMap;

/// The environment in which expressions are evaluated (variable assignments made using `with`). 
/// This is passed as an argument to [`Expr::eval()`].
#[derive(Clone)]
pub struct Env {
    pub vars: Rc<HashMap<String, Rhs>>,
    pub alpha: Rc<Alphabet>,
    pub tracer: Rc<RefCell<dyn Tracer>>,
}

impl Env {
    pub(crate) fn wrap_describe(&self, call: impl FnOnce(u32, &Env) -> String, prec: u32, env_outer: &Env) -> String {
        self.alpha.wrap_describe(|prec, env|
            if Rc::ptr_eq(&self.vars, &env.vars) || self.vars.is_empty() && env.vars.is_empty() {
                call(prec, self)
            } else {
                format!("with({}, {})", self.describe(env), call(0, self))
            },
            prec, env_outer)
    }

    /// The alphabet used for ordering characters and arithmetic operations on them.
    pub fn alphabet(&self) -> &Rc<Alphabet> { &self.alpha }

    fn describe(&self, env: &Env) -> String {
        let mut iter = self.vars.iter()
            .map(|(key, val)| match val {
                Rhs::Value(item) => format!("{}={}", key, item.describe_inner(1, env)),
                Rhs::Function(expr, saved_env) => format!("{}={{{}}}", key, expr.describe_inner(0, saved_env)) // TODO wrap
            });
        let mut ret = match iter.next() {
            Some(rec) => rec,
            None => return String::new()
        };
        for rec in iter {
            ret += ", ";
            ret += &rec;
        }
        ret
    }
}

impl Default for Env {
    fn default() -> Env {
        Env {
            vars: Default::default(),
            alpha: Default::default(),
            tracer: Rc::new(RefCell::new(()))
        }
    }
}

#[derive(Clone)]
pub enum Rhs {
    Value(Item),
    Function(Expr, Env)
}

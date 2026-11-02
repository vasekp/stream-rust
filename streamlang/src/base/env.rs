use crate::base::*;

pub use std::rc::{Rc, Weak};

/// The lexical context in which expressions are evaluated: currently just the alphabet in effect
/// (set using `alpha`). This is passed as an argument to [`Expr::eval()`].
/// Variables are not part of it: `with` substitutes them into its body before evaluation. A
/// function variable captures the `Env` of its definition as a [`Closure`].
#[derive(Clone, Default, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Env {
    pub alpha: Rc<Alphabet>,
}

impl Env {
    pub(crate) fn wrap_describe(outer: &Env, inner: &Env, call: impl FnOnce(u32) -> String, prec: u32) -> String {
        if Rc::ptr_eq(&outer.alpha, &inner.alpha)
            || (matches!(*outer.alpha, Alphabet::Std26) && matches!(*inner.alpha, Alphabet::Std26))
        {
            call(prec)
        } else {
            format!("alpha({}, {})", inner.alpha.describe(), call(0))
        }
    }
}

/// The right-hand side of a variable assignment, in `with` or at the session level
/// (`$name = ...`): either a value, or a function (`{...}`, closed over the environment of the
/// assignment) which receives the source and arguments of each use of the variable.
#[derive(Clone)]
pub enum Rhs {
    Value(Item),
    Function(Closure),
}

impl Describe for Rhs {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        match self {
            Rhs::Value(item) => item.describe_inner(prec, env),
            Rhs::Function(closure) => closure.describe_inner(prec, env),
        }
    }
}

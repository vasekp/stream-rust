use crate::base::*;
use super::tracing::Tracer;

pub use std::rc::{Rc, Weak};
use std::cell::RefCell;

/// The context in which expressions are evaluated: the alphabet in effect (set using `alpha`) and
/// the tracer receiving evaluation events. This is passed as an argument to [`Expr::eval()`].
/// Variables are not part of it: `with` substitutes them into its body before evaluation.
#[derive(Clone)]
pub struct Env {
    pub alpha: Rc<Alphabet>,
    pub tracer: Rc<RefCell<dyn Tracer>>,
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

    pub(crate) fn base(&self) -> Self {
        Env { alpha: Default::default(), tracer: Rc::clone(&self.tracer) }
    }
}

impl Default for Env {
    fn default() -> Env {
        Env { alpha: Default::default(), tracer: Rc::new(RefCell::new(())) }
    }
}

/// The right-hand side of a variable assignment, in `with` or at the session level
/// (`$name = ...`): either a value, or a function body (`{...}`) which receives the source and
/// arguments of each use of the variable.
#[derive(Clone)]
pub enum Rhs {
    Value(Item),
    Function(Expr),
}

impl Describe for Rhs {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        match self {
            Rhs::Value(item) => item.describe_inner(prec, env),
            Rhs::Function(expr) => format!("{{{}}}", expr.describe_inner(prec, env)),
        }
    }
}

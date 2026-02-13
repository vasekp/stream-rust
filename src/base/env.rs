use crate::base::*;
use super::tracing::Tracer;

pub use std::rc::{Rc, Weak};
use std::cell::RefCell;

/// The environment in which expressions are evaluated (variable assignments made using `with`). 
/// This is passed as an argument to [`Expr::eval()`].
#[derive(Clone)]
pub struct Env {
    pub alpha: Rc<Alphabet>,
    pub tracer: Rc<RefCell<dyn Tracer>>,
}

impl Env {
    pub(crate) fn wrap_describe(outer: &Env, inner: &Env, call: impl FnOnce(u32) -> String, prec: u32) -> String {
        if Rc::ptr_eq(&outer.alpha, &inner.alpha) || (matches!(*outer.alpha, Alphabet::Std26) &&
        matches!(*inner.alpha, Alphabet::Std26)) {
            call(prec)
        } else {
            format!("alpha({}, {})", inner.alpha.describe(), call(0))
        }
    }
}

impl Default for Env {
    fn default() -> Env {
        Env {
            alpha: Default::default(),
            tracer: Rc::new(RefCell::new(()))
        }
    }
}

#[derive(Clone)]
pub enum Rhs {
    Value(Item),
    Function(Expr)
}

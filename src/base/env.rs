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
    pub(crate) fn wrap_describe(&self, call: impl FnOnce(u32, &Env) -> String, prec: u32, env: &Env) -> String {
        self.alpha.wrap_describe(|prec, _| call(prec, self), prec, env) // TODO check?
    }

    /// The alphabet used for ordering characters and arithmetic operations on them.
    pub fn alphabet(&self) -> &Rc<Alphabet> { &self.alpha }
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

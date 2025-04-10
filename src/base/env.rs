use crate::base::*;

pub use std::rc::{Rc, Weak};

/// The environment in which expressions are evaluated. This is passed as an argument to
/// [`Expr::eval()`]. Currently a placeholder, but in the future to be defined through `env`.
#[derive(Default, Clone)]
pub struct Env {
    pub(crate) cache: std::rc::Weak<crate::ops::selfref::CacheHistory>
}

impl Env {
    fn is_trivial(&self) -> bool { true }

    pub(crate) fn wrap_describe(&self, call: impl FnOnce(u32) -> String, prec: u32) -> String {
        match self.is_trivial() {
            true => call(prec),
            false => format!("env({}, {})", self.describe(), call(0))
        }
    }

    /// The alphabet used for ordering characters and arithmetic operations on them.
    pub fn alphabet(&self) -> &Alphabet { &Alphabet::Std26 }

    fn describe(&self) -> String { todo!() }
}

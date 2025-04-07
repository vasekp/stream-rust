use crate::base::*;

/// The environment in which expressions are evaluated. This is passed as an argument to
/// [`Expr::eval()`]. Currently a placeholder, but in the future to be defined through `env`.
#[derive(Default, Clone)]
pub struct Env {
    pub(crate) cache: std::rc::Weak<crate::ops::selfref::CacheHistory>
}

impl Env {
    fn is_trivial(&self) -> bool { true }

    pub(crate) fn wrap_describe(&self, expr: impl Into<String> + std::fmt::Display) -> String {
        match self.is_trivial() {
            true => expr.into(),
            false => format!("env({}, {})", self.describe(), expr)
        }
    }

    /// The alphabet used for ordering characters and arithmetic operations on them.
    pub fn alphabet(&self) -> &Alphabet { &Alphabet::Std26 }
}

impl Describe for Env {
    fn describe(&self) -> String { todo!() }
}

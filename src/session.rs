use crate::base::*;

/// A `Session` holds information necessary for evaluating symbolic expressions. This includes a
/// register of defined symbols.
pub struct Session {
}

impl Session {
    /// Create a new `Session` and initialize the predefined symbols register.
    pub fn new() -> Session {
        Session { }
    }

    /// A call to `eval` evaluates an [`Expr`] into an [`Item`]. This is potentially
    /// context-dependent through symbol assignments or history, and thus a function of `Session`.
    pub fn process(&self, expr: Expr) -> Result<Item, StreamError> {
        match expr {
            Expr::Imm(item) => Ok(item),
            Expr::Eval(node) => node.eval()
        }
    }
}

impl Default for Session {
    fn default() -> Self {
        Self::new()
    }
}

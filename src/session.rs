use crate::base::*;
use crate::keywords::find_keyword;

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
    pub fn eval(&self, expr: Expr) -> Result<Item, StreamError> {
        match expr {
            Expr::Imm(item) => Ok(item),
            Expr::Eval(node) => match node.head {
                Head::Symbol(ref sym) | Head::Oper(ref sym) => match find_keyword(sym) {
                    Ok(func) => func(self, node),
                    Err(e) => Err(e.with_node(node))
                },
                _ => Err(StreamError::new("not implemented", node))
            }
        }
    }
}

impl Default for Session {
    fn default() -> Self {
        Self::new()
    }
}

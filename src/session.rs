use std::collections::HashMap;
use crate::base::*;
use crate::{lang, ops};

type Constructor = fn(&Session, Node) -> Result<Item, StreamError>;

/// A `Session` holds information necessary for evaluating symbolic expressions. This includes a
/// register of defined symbols.
pub struct Session {
    register: HashMap<&'static str, Constructor>
}

impl Session {
    /// Create a new `Session` and initialize the predefined symbols register.
    pub fn new() -> Session {
        let mut s = Session{ register: Default::default() };
        lang::init(&mut s);
        ops::init(&mut s);
        s
    }

    pub(crate) fn register_symbol(&mut self, name: &'static str, ctor: Constructor) {
        self.register.insert(name, ctor);
    }

    fn find_symbol(&self, name: &str) -> Result<Constructor, StreamError> {
        self.register.get(name).copied()
            .ok_or_else(|| StreamError::from(format!("symbol '{name}' not found")))
    }

    /// A call to `eval` evaluates an [`Expr`] into an [`Item`]. This is potentially
    /// context-dependent through symbol assignments or history, and thus a function of `Session`.
    pub fn eval(&self, expr: Expr) -> Result<Item, StreamError> {
        match expr {
            Expr::Imm(item) => Ok(item),
            Expr::Eval(node) => match node.head {
                Head::Symbol(ref sym) | Head::Oper(ref sym) => match self.find_symbol(sym) {
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

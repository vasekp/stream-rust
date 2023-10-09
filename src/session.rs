use std::collections::HashMap;
use crate::base::*;
use crate::{lang, ops};

type Constructor = fn(&Session, &Node) -> Result<Item, BaseError>;

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

    fn find_symbol(&self, name: &str) -> Result<Constructor, BaseError> {
        self.register.get(name).copied()
            .ok_or_else(|| BaseError::from(format!("symbol '{name}' not found")))
    }

    /// A call to `eval` evaluates an [`Expr`] into an [`Item`]. This is potentially
    /// context-dependent through symbol assignments or history, and thus a function of `Session`.
    pub fn eval(&self, expr: &Expr) -> Result<Item, BaseError> {
        match expr {
            Expr::Imm(item) => Ok(item.clone()),
            Expr::Eval(node) => match &node.core {
                Core::Symbol(sym) => {
                    let func = self.find_symbol(&sym)?;
                    func(self, node)
                },
                _ => Err(BaseError::from("not implemented"))
            }
        }
    }
}

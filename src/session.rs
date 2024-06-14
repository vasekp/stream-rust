use std::collections::HashMap;
use crate::base::*;
use crate::{lang, ops};

type Constructor = fn(&Session, Node) -> Result<Item, StreamError>;

pub(crate) type Keywords = HashMap<&'static str, Constructor>;

static mut KEYWORDS: Option<Keywords> = None;

pub(crate) fn find_keyword(name: &str) -> Result<Constructor, StreamError> {
    let keywords = unsafe {
        if KEYWORDS.is_none() {
            let mut keywords = Default::default();
            lang::init(&mut keywords);
            ops::init(&mut keywords);
            KEYWORDS = Some(keywords);
        }
        KEYWORDS.as_ref().unwrap()
    };
    keywords.get(name).copied()
        .ok_or_else(|| StreamError::from(format!("symbol '{name}' not found")))
}

/// A `Session` holds information necessary for evaluating symbolic expressions. This includes a
/// register of defined symbols.
pub struct Session {
}

impl Session {
    /// Create a new `Session` and initialize the predefined symbols register.
    pub fn new() -> Session {
        Session { }
    }

    fn find_symbol(&self, name: &str) -> Result<Constructor, StreamError> {
        find_keyword(name)
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

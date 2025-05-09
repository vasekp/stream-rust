use crate::base::*;

use std::collections::HashMap;

/// A `Session` holds information necessary for evaluating symbolic expressions. This includes a
/// register of defined symbols.
pub struct Session {
    hist: Vec<Item>,
    vars: HashMap<String, Item>,
}

impl Session {
    /// Create a new `Session` and initialize the predefined symbols register.
    pub fn new() -> Session {
        Session {
            hist: Vec::new(),
            vars: HashMap::new(),
        }
    }

    /// A call to `eval` evaluates an [`Expr`] into an [`Item`]. This is potentially
    /// context-dependent through symbol assignments or history, and thus a function of `Session`.
    pub fn process(&mut self, mut expr: Expr) -> Result<SessionUpdate<'_>, StreamError> {
        expr.replace(&|subs| {
            if subs.kind == SubstKind::History {
                match subs.index {
                    Some(ix @ 1..) =>
                        self.hist.get(ix - 1)
                            .cloned()
                            .map(Expr::from)
                            .ok_or(format!("history item %{ix} does not exist").into()),
                    Some(0) => Err(BaseError::from("invalid history index")),
                    None =>
                        self.hist.last()
                            .cloned()
                            .map(Expr::from)
                            .ok_or("history is empty".into())
                }
            } else {
                Ok(Expr::Repl(subs))
            }
        })?;
        match expr {
            Expr::Eval(Node { head: Head::Oper(op), source: None, mut args }) if op == "=" => {
                let item = args.pop()
                    .expect("= should have at least 2 args")
                    .eval_default()?;
                let mut names = Vec::with_capacity(args.len());
                for arg in args {
                    let name = match arg {
                        Expr::Eval(Node { head: Head::Symbol(sym), source: None, args })
                            if args.is_empty() && sym.starts_with('$')
                        => sym,
                        _ => return Err(StreamError::new("expected global variable ($name)", arg))
                    };
                    names.push(name);
                }
                let last = names.pop();
                for name in &names {
                    self.vars.insert(name.to_owned(), item.clone());
                }
                if let Some(name) = last {
                    self.vars.insert(name.to_owned(), item);
                    names.push(name);
                }
                Ok(SessionUpdate::Globals(names))
            },
            Expr::Eval(Node { head: Head::Symbol(sym), source: None, args }) if sym == "clear" => {
                let mut names = Vec::with_capacity(args.len());
                let mut updated = Vec::with_capacity(args.len());
                for arg in args {
                    let name = match arg {
                        Expr::Eval(Node { head: Head::Symbol(sym), source: None, args })
                            if args.is_empty() && sym.starts_with('$')
                        => sym,
                        _ => return Err(StreamError::new("expected global variable ($name)", arg))
                    };
                    names.push(name);
                }
                for name in names {
                    if self.vars.remove(&name).is_some() {
                        updated.push(name);
                    }
                }
                Ok(SessionUpdate::Globals(updated))
            },
            _ => {
                let item = expr.eval_default()?;
                self.hist.push(item);
                Ok(SessionUpdate::History(self.hist.len(), self.hist.last().expect("should be nonempty after push()")))
            }
        }
    }
}

impl Default for Session {
    fn default() -> Self {
        Self::new()
    }
}

pub enum SessionUpdate<'a> {
    History(usize, &'a Item),
    Globals(Vec<String>),
}

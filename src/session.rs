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
    pub fn process(&mut self, expr: Expr) -> Result<SessionUpdate<'_>, StreamError> {
        let expr = expr.replace(&|sub_expr| {
            match sub_expr {
                Expr::Repl(Subst { kind: SubstKind::History, index }) => match index {
                    Some(ix @ 1..) => Ok(try_with!(sub_expr,
                        self.hist.get(ix - 1)
                            .cloned()
                            .map(Expr::from)
                            .ok_or(format!("history item %{ix} does not exist"))?)),
                    Some(0) => Err(StreamError::new("invalid history index", sub_expr)),
                    None => Ok(try_with!(sub_expr,
                        self.hist.last()
                            .cloned()
                            .map(Expr::from)
                            .ok_or("history is empty")?))
                },
                Expr::Eval(node) => match node {
                    Node { head: Head::Symbol(ref sym), ref source, ref args } if sym.starts_with('$') => {
                        if source.is_some() || !args.is_empty() {
                            return Err(StreamError::new("no source or arguments allowed", node));
                        }
                        match self.vars.get(sym) {
                            Some(item) => Ok(Expr::Imm(item.clone())),
                            None => Err(StreamError::new(format!("variable {sym} not defined"), node))
                        }
                    },
                    _ => Ok(node.into())
                },
                _ => Ok(sub_expr)
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

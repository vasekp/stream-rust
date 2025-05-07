use crate::base::*;

/// A `Session` holds information necessary for evaluating symbolic expressions. This includes a
/// register of defined symbols.
pub struct Session {
    hist: Vec<Item>,
}

impl Session {
    /// Create a new `Session` and initialize the predefined symbols register.
    pub fn new() -> Session {
        Session {
            hist: Vec::new(),
        }
    }

    /// A call to `eval` evaluates an [`Expr`] into an [`Item`]. This is potentially
    /// context-dependent through symbol assignments or history, and thus a function of `Session`.
    pub fn process(&mut self, mut expr: Expr) -> Result<(usize, &Item), StreamError> {
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
                            .ok_or(format!("history is empty").into())
                }
            } else {
                Ok(Expr::Repl(subs))
            }
        })?;
        let item = expr.eval_default()?;
        self.hist.push(item);
        Ok((self.hist.len(), self.hist.last().expect("should be nonempty after push()")))
    }
}

impl Default for Session {
    fn default() -> Self {
        Self::new()
    }
}

use crate::base::*;
use crate::base::tracing::Tracer;

use std::collections::HashMap;

/// A `Session` holds information necessary for evaluating symbolic expressions. This includes a
/// register of defined symbols.
pub struct Session {
    hist: Vec<Item>,
    vars: HashMap<String, Rhs>,
    env: Env,
    counter: usize,
}

impl Session {
    /// Create a new `Session` and initialize the predefined symbols register.
    pub fn new() -> Session {
        Session {
            hist: Vec::new(),
            vars: HashMap::new(),
            env: Default::default(),
            counter: 1,
        }
    }

    /// A call to `eval` evaluates an [`Expr`] into an [`Item`]. This is potentially
    /// context-dependent through symbol assignments or history, and thus a function of `Session`.
    pub fn process(&mut self, expr: Expr) -> Result<SessionUpdate<'_>, StreamError> {
        stop::reset_stop();
        match expr {
            Expr::Eval(Node { head: Head::Oper(op), source: None, mut args }) if op == "=" => {
                let rhs = args.pop().expect("= should have at least 2 args");
                let rhs = match self.apply_context(rhs)? {
                    Expr::Eval(Node { head: Head::Block(block), source: None, args }) if args.is_empty()
                        => Rhs::Function(*block),
                    expr => Rhs::Value(expr.eval(&self.env)?)
                };
                let mut names = Vec::with_capacity(args.len());
                for arg in args {
                    let name = match arg {
                        Expr::Eval(Node { head: Head::Symbol(ref sym), source: None, ref args })
                            if args.is_empty() && sym.starts_with('$') => {
                                if sym.as_bytes()[1].is_ascii_digit() {
                                    return Err(StreamError::new("reserved variable name", arg));
                                } else {
                                    sym
                                }
                            },
                        _ => return Err(StreamError::new("expected global variable ($name)", arg))
                    };
                    names.push(name.to_owned());
                }
                let last = names.pop();
                for name in &names {
                    self.vars.insert(name.to_owned(), rhs.clone());
                }
                if let Some(name) = last {
                    self.vars.insert(name.to_owned(), rhs);
                    names.push(name);
                }
                self.counter += 1;
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
                self.counter += 1;
                Ok(SessionUpdate::Globals(updated))
            },
            _ => {
                let item = self.apply_context(expr)?.eval(&self.env)?;
                self.hist.push(item);
                self.counter += 1;
                Ok(SessionUpdate::History(self.hist.len(), self.hist.last().expect("should be nonempty after push()")))
            }
        }
    }

    fn apply_context(&mut self, expr: Expr) -> Result<Expr, StreamError> {
        expr.replace(&|sub_expr| {
            use std::ops::ControlFlow;
            match sub_expr {
                Expr::Repl(Subst::History(index)) => match index {
                    Some(ix @ 1..) => Ok(ControlFlow::Break(try_with!(sub_expr,
                        self.hist.get(ix - 1)
                            .cloned()
                            .map(Expr::from)
                            .ok_or(format!("history item %{ix} does not exist"))?))),
                    Some(0) => Err(StreamError::new("invalid history index", sub_expr)),
                    None => Ok(ControlFlow::Break(try_with!(sub_expr,
                        self.hist.last()
                            .cloned()
                            .map(Expr::from)
                            .ok_or("history is empty")?))),
                },
                Expr::Repl(Subst::Counter) =>
                    Ok(ControlFlow::Break(Expr::new_number(self.counter))),
                Expr::Eval(mut node) => {
                    match node {
                        Node { head: Head::Symbol(ref sym), ref mut source, ref mut args } if sym.starts_with('$') => {
                            match self.vars.get(sym) {
                                Some(Rhs::Value(item)) => {
                                    if source.is_some() || !args.is_empty() {
                                        Err(StreamError::new("no source or arguments allowed", node))
                                    } else {
                                        Ok(ControlFlow::Break(Expr::Imm(item.clone())))
                                    }
                                },
                                Some(Rhs::Function(block)) => {
                                    Ok(ControlFlow::Break(Expr::Eval(Node {
                                        head: Expr::new_node("global", vec![block.clone()]).into(),
                                        source: source.take(),
                                        args: std::mem::take(args)
                                    })))
                                },
                                None => Err(StreamError::new("variable not defined", node))
                            }
                        },
                        _ => Ok(ControlFlow::Continue(node))
                    }
                },
                sub_expr @ (Expr::Imm(_) | Expr::Repl(_)) => Ok(ControlFlow::Break(sub_expr))
            }
        })
    }

    pub fn history(&self) -> &Vec<Item> {
        &self.hist
    }

    pub fn set_tracer(&mut self, tracer: impl Tracer + 'static) {
        self.env.tracer = Rc::new(std::cell::RefCell::new(tracer));
    }
}

impl Default for Session {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum SessionUpdate<'a> {
    History(usize, &'a Item),
    Globals(Vec<String>),
}

impl<'a> SessionUpdate<'a> {
    #[allow(unused)]
    fn unwrap(self) -> &'a Item {
        match self {
            Self::History(_, item) => item,
            _ => panic!("SessionUpdate::unwrap on non-History")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn test_session() {
        let mut sess = Session::new();
        assert_eq!(sess.process(parse("$a=$b=10").unwrap()).unwrap(), SessionUpdate::Globals(vec!["$a".into(), "$b".into()]));
        assert_eq!(sess.process(parse("$a=$a+$b").unwrap()).unwrap(), SessionUpdate::Globals(vec!["$a".into()]));
        assert_eq!(sess.process(parse("clear($b)").unwrap()).unwrap(), SessionUpdate::Globals(vec!["$b".into()]));
        assert_eq!(sess.process(parse("$a").unwrap()).unwrap(), SessionUpdate::History(1, &Item::new_number(20)));
        assert!(sess.process(parse("$b").unwrap()).is_err());
        assert!(sess.process(parse("$c={$c}").unwrap()).is_err());

        let mut sess = Session::new();
        assert_eq!(sess.process(parse("100").unwrap()).unwrap(), SessionUpdate::History(1, &Item::new_number(100)));
        assert_eq!(sess.process(parse("% * %1").unwrap()).unwrap(), SessionUpdate::History(2, &Item::new_number(10000)));
        assert_eq!(sess.process(parse("% + %1").unwrap()).unwrap(), SessionUpdate::History(3, &Item::new_number(10100)));

        let mut sess = Session::new();
        assert_eq!(sess.process(parse("$a={#+#1*#2}").unwrap()).unwrap(), SessionUpdate::Globals(vec!["$a".into()]));
        assert_eq!(sess.process(parse("5.$a(6,7)").unwrap()).unwrap().unwrap(), &Item::new_number(47));
        assert_eq!(sess.process(parse("{5.$a(6,7)}").unwrap()).unwrap().unwrap(), &Item::new_number(47));
        assert_eq!(sess.process(parse("5.$a@[6,7]").unwrap()).unwrap().unwrap(), &Item::new_number(47));
        assert_eq!(sess.process(parse("5.{#.$a(#1,#2)}(6,7)").unwrap()).unwrap().unwrap(), &Item::new_number(47));
        assert_eq!(sess.process(parse("5.{#.$a(#1,#2)}@[6,7]").unwrap()).unwrap().unwrap(), &Item::new_number(47));

        let mut sess = Session::new();
        assert_eq!(sess.process(parse("$a={a}").unwrap()).unwrap(), SessionUpdate::Globals(vec!["$a".into()]));
        assert_eq!(sess.process(parse("with(a=1,{a})").unwrap()).unwrap().unwrap(), &Item::new_number(1));
        assert!(sess.process(parse("with(a=1,$a)").unwrap()).is_err());
        assert!(sess.process(parse("$1=1").unwrap()).is_err());

        let mut sess = Session::new();
        assert_eq!(sess.process(parse("$#").unwrap()).unwrap().unwrap(), &Item::new_number(1));
        assert_eq!(sess.process(parse("$#").unwrap()).unwrap().unwrap(), &Item::new_number(2));
        assert_eq!(sess.process(parse("$a=$#").unwrap()).unwrap(), SessionUpdate::Globals(vec!["$a".into()]));
        assert_eq!(sess.process(parse("$#").unwrap()).unwrap().unwrap(), &Item::new_number(4));
        assert_eq!(sess.process(parse("$a").unwrap()).unwrap().unwrap(), &Item::new_number(3));
        assert_eq!(sess.process(parse("$a").unwrap()).unwrap().unwrap(), &Item::new_number(3));
    }
}

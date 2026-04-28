use crate::base::*;
use crate::base::tracing::Tracer;

use std::collections::HashMap;
use std::borrow::Cow;

/// A `Session` holds information necessary for evaluating symbolic expressions. This includes a
/// register of defined symbols.
pub struct Session {
    hist: Vec<Item>,
    vars: HashMap<&'static str, Rhs>,
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
    pub fn process(&mut self, expr: Expr) -> SResult<SessionUpdate<'_>> {
        match &expr {
            Expr::Eval(node) => match &**node {
                Node { head: Head::Oper("="), source: None, args } => {
                    let Some((rhs, lhs)) = args.split_last() else { unreachable!() };
                    let rhs = self.apply_context(rhs)?;
                    let rhs = if let Expr::Eval(node) = &*rhs
                        && node.source.is_none() && node.args.is_empty()
                        && let Head::Block{body, ..} = &node.head {
                            Rhs::Function(body.clone())
                    } else {
                        Rhs::Value(rhs.eval(&self.env)?)
                    };
                    let mut names = Vec::with_capacity(lhs.len());
                    for arg in lhs {
                        let name = if let Expr::Eval(node) = arg
                            && node.source.is_none() && node.args.is_empty()
                            && let Head::Symbol(sym) = &node.head
                            && sym.starts_with('$') {
                                if sym.as_bytes()[1].is_ascii_digit() {
                                    return Err(StreamError::with_expr("reserved variable name", arg));
                                } else {
                                    *sym
                                }
                            } else {
                                return Err(StreamError::with_expr("expected global variable ($name)", arg));
                            };
                        names.push(name);
                    }
                    let last = names.pop();
                    for name in &names {
                        self.vars.insert(name, rhs.clone());
                    }
                    if let Some(name) = last {
                        self.vars.insert(name, rhs);
                        names.push(name);
                    }
                    self.counter += 1;
                    Ok(SessionUpdate::Globals(names))
                },
                Node { head: Head::Symbol("clear"), source: None, args } => {
                    let mut names = Vec::with_capacity(args.len());
                    let mut updated = Vec::with_capacity(args.len());
                    for arg in args {
                        let name = if let Expr::Eval(node) = arg
                            && node.source.is_none() && node.args.is_empty()
                            && let Head::Symbol(sym) = &node.head
                            && sym.starts_with('$') {
                                if sym.as_bytes()[1].is_ascii_digit() {
                                    return Err(StreamError::with_expr("reserved variable name", arg));
                                } else {
                                    *sym
                                }
                            } else {
                                return Err(StreamError::with_expr("expected global variable ($name)", arg));
                            };
                        names.push(name);
                    }
                    for name in names {
                        if self.vars.remove(name).is_some() {
                            updated.push(name);
                        }
                    }
                    self.counter += 1;
                    Ok(SessionUpdate::Globals(updated))
                },
                _ => {
                    let item = self.apply_context(&expr)?.eval(&self.env)?;
                    self.hist.push(item);
                    self.counter += 1;
                    Ok(SessionUpdate::History(self.hist.len(), self.hist.last().expect("should be nonempty after push()")))
                }
            },
            _ => {
                let item = self.apply_context(&expr)?.eval(&self.env)?;
                self.hist.push(item);
                self.counter += 1;
                Ok(SessionUpdate::History(self.hist.len(), self.hist.last().expect("should be nonempty after push()")))
            }
        }
    }

    fn apply_context<'a>(&self, expr: &'a Expr) -> SResult<Cow<'a, Expr>> {
        match expr {
            Expr::Repl(Subst::History(index)) => match index {
                Some(ix @ 1..) => Ok(Cow::Owned(
                    self.hist.get(ix - 1)
                        .cloned()
                        .map(Expr::from)
                        .ok_or(StreamError::with_expr("history item does not exist", expr))?)),
                Some(0) => Err(StreamError::with_expr("invalid history index", expr)),
                None => Ok(Cow::Owned(
                    self.hist.last()
                        .cloned()
                        .map(Expr::from)
                        .ok_or(StreamError::with_expr("history is empty", expr))?)),
            },
            Expr::Repl(Subst::Counter) =>
                Ok(Cow::Owned(Expr::new_number(self.counter))),
            Expr::Eval(node) => {
                let mut nnode = Cow::Borrowed(&**node);
                if let Head::Symbol(sym) = &node.head && sym.starts_with('$') {
                    match self.vars.get(sym) {
                        Some(Rhs::Value(item)) => {
                            return if node.source.is_none() && node.args.is_empty() {
                                Ok(Cow::Owned(Expr::Imm(item.clone())))
                            } else {
                                Err(StreamError::with_expr("no source or arguments allowed", expr))
                            }
                        },
                        Some(Rhs::Function(body)) => {
                            nnode.to_mut().head = Head::Block{body: body.clone(), reset_env: true};
                        },
                        None => return Err(StreamError::with_expr("variable not defined", expr))
                    }
                } else if let Head::Block{body, reset_env} = &node.head
                    && let Cow::Owned(new_body) = self.apply_context(body)? {
                        nnode.to_mut().head = Head::Block{body: new_body, reset_env: *reset_env};
                }
                if let Some(source) = &node.source
                    && let Cow::Owned(new_source) = self.apply_context(source)? {
                        nnode.to_mut().source = Some(new_source);
                }
                for (ix, arg) in node.args.iter().enumerate() {
                    if let Cow::Owned(new_arg) = self.apply_context(arg)? {
                        nnode.to_mut().args[ix] = new_arg;
                    }
                }
                match nnode {
                    Cow::Borrowed(_) => Ok(Cow::Borrowed(expr)),
                    Cow::Owned(node) => Ok(Cow::Owned(node.into())),
                }
            },
            _ => Ok(Cow::Borrowed(expr))
        }
    }

    pub fn history(&self) -> &Vec<Item> {
        &self.hist
    }

    pub fn set_tracer(&mut self, tracer: Rc<std::cell::RefCell<impl Tracer + 'static>>) {
        self.env.tracer = tracer;
    }

    pub fn vars(&self) -> &HashMap<&'static str, Rhs> {
        &self.vars
    }

    pub fn vars_mut(&mut self) -> &mut HashMap<&'static str, Rhs> {
        &mut self.vars
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
    Globals(Vec<&'static str>),
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
        assert_eq!(sess.process(parse("$a=$b=10").unwrap()).unwrap(), SessionUpdate::Globals(vec!["$a", "$b"]));
        assert_eq!(sess.process(parse("$a=$a+$b").unwrap()).unwrap(), SessionUpdate::Globals(vec!["$a"]));
        assert_eq!(sess.process(parse("clear($b)").unwrap()).unwrap(), SessionUpdate::Globals(vec!["$b"]));
        assert_eq!(sess.process(parse("$a").unwrap()).unwrap(), SessionUpdate::History(1, &Item::new_number(20)));
        assert!(sess.process(parse("$b").unwrap()).is_err());
        assert!(sess.process(parse("$c={$c}").unwrap()).is_err());

        let mut sess = Session::new();
        assert_eq!(sess.process(parse("100").unwrap()).unwrap(), SessionUpdate::History(1, &Item::new_number(100)));
        assert_eq!(sess.process(parse("% * %1").unwrap()).unwrap(), SessionUpdate::History(2, &Item::new_number(10000)));
        assert_eq!(sess.process(parse("% + %1").unwrap()).unwrap(), SessionUpdate::History(3, &Item::new_number(10100)));

        let mut sess = Session::new();
        assert_eq!(sess.process(parse("$a={#+#1*#2}").unwrap()).unwrap(), SessionUpdate::Globals(vec!["$a"]));
        assert_eq!(sess.process(parse("5.$a(6,7)").unwrap()).unwrap().unwrap(), &Item::new_number(47));
        assert_eq!(sess.process(parse("{5.$a(6,7)}").unwrap()).unwrap().unwrap(), &Item::new_number(47));
        assert_eq!(sess.process(parse("5.$a@[6,7]").unwrap()).unwrap().unwrap(), &Item::new_number(47));
        assert_eq!(sess.process(parse("5.{#.$a(#1,#2)}(6,7)").unwrap()).unwrap().unwrap(), &Item::new_number(47));
        assert_eq!(sess.process(parse("5.{#.$a(#1,#2)}@[6,7]").unwrap()).unwrap().unwrap(), &Item::new_number(47));

        let mut sess = Session::new();
        assert_eq!(sess.process(parse("$a={a}").unwrap()).unwrap(), SessionUpdate::Globals(vec!["$a"]));
        assert_eq!(sess.process(parse("with(a=1,{a})").unwrap()).unwrap().unwrap(), &Item::new_number(1));
        assert!(sess.process(parse("with(a=1,$a)").unwrap()).is_err());
        assert!(sess.process(parse("$1=1").unwrap()).is_err());

        let mut sess = Session::new();
        assert_eq!(sess.process(parse("$#").unwrap()).unwrap().unwrap(), &Item::new_number(1));
        assert_eq!(sess.process(parse("$#").unwrap()).unwrap().unwrap(), &Item::new_number(2));
        assert_eq!(sess.process(parse("$a=$#").unwrap()).unwrap(), SessionUpdate::Globals(vec!["$a"]));
        assert_eq!(sess.process(parse("$#").unwrap()).unwrap().unwrap(), &Item::new_number(4));
        assert_eq!(sess.process(parse("$a").unwrap()).unwrap().unwrap(), &Item::new_number(3));
        assert_eq!(sess.process(parse("$a").unwrap()).unwrap().unwrap(), &Item::new_number(3));
    }
}

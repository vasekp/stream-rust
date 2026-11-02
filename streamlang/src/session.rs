use crate::base::*;
use crate::base::stop;
use crate::base::tracing::{self, Tracer};
use std::cell::RefCell;

use std::collections::HashMap;
use std::borrow::Cow;

/// A `Session` holds the state which makes evaluation context-dependent: the history of results
/// (`%`, `%n`), the global variables (`$name`), the evaluation [`Env`], the input counter
/// (`$#`) and the tracer, if any.
pub struct Session {
    hist: Vec<Item>,
    vars: HashMap<&'static str, Rhs>,
    env: Env,
    counter: usize,
    tracer: Option<Rc<RefCell<dyn Tracer>>>,
}

impl Session {
    /// Creates a new `Session` with empty history and variables and the default environment.
    pub fn new() -> Session {
        Session { hist: Vec::new(), vars: HashMap::new(), env: Default::default(), counter: 1, tracer: None }
    }

    /// A call to `eval` evaluates an [`Expr`] into an [`Item`]. This is potentially
    /// context-dependent through symbol assignments or history, and thus a function of `Session`.
    ///
    /// Clears any pending interrupt request (see [`stop::send_stop()`]) before evaluating, so that
    /// a stale request from an earlier evaluation cannot affect this one. The tracer set by
    /// [`set_tracer()`](Self::set_tracer) is installed for the duration of this call only: iterating
    /// a returned stream afterwards is not traced.
    pub fn process(&mut self, expr: Expr) -> SResult<SessionUpdate<'_>> {
        stop::reset_stop();
        let _tracer = self.tracer.as_ref().map(|tracer| tracing::install(Rc::clone(tracer)));
        match &expr {
            Expr::Eval(node) if &node.head == "=" && node.source.is_none() => self.assign(&node.args),
            Expr::Eval(node) if &node.head == "clear" && node.source.is_none() => self.clear(&node.args),
            _ => self.evaluate(&expr),
        }
    }

    /// Handles `$a = $b = ... = rhs`. The `args` are the operands of `=`, so the last one is the
    /// right-hand side and all the others must be global variable names.
    fn assign(&mut self, args: &[Expr]) -> SResult<SessionUpdate<'_>> {
        let Some((rhs, lhs)) = args.split_last() else {
            unreachable!("= should have at least 2 arguments by construction")
        };
        let rhs = self.apply_context(rhs)?;
        let rhs = if let Expr::Eval(node) = &*rhs
            && node.source.is_none()
            && node.args.is_empty()
            && let Some(closure) = match &node.head {
                Head::Block(body) => Some(Closure::new(body.clone(), self.env.clone())),
                Head::Closure(closure) => Some(closure.clone()),
                _ => None,
            }
        {
            Rhs::Function(closure)
        } else {
            Rhs::Value(rhs.eval(&self.env)?)
        };
        let mut names = lhs.iter().map(Self::global_name).collect::<SResult<Vec<_>>>()?;
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
    }

    /// Handles `clear($a, $b, ...)`. Reports only the names which were actually defined.
    fn clear(&mut self, args: &[Expr]) -> SResult<SessionUpdate<'_>> {
        let names = args.iter().map(Self::global_name).collect::<SResult<Vec<_>>>()?;
        let mut updated = Vec::with_capacity(names.len());
        for name in names {
            if self.vars.remove(name).is_some() {
                updated.push(name);
            }
        }
        self.counter += 1;
        Ok(SessionUpdate::Globals(updated))
    }

    /// Evaluates any other expression and appends the result to the history.
    fn evaluate(&mut self, expr: &Expr) -> SResult<SessionUpdate<'_>> {
        let item = self.apply_context(expr)?.eval(&self.env)?;
        self.hist.push(item);
        self.counter += 1;
        Ok(SessionUpdate::History(self.hist.len(), self.hist.last().expect("should be nonempty after push()")))
    }

    /// Checks that `expr` is a bare global variable name (`$name`, not `$123`) and returns it.
    fn global_name(expr: &Expr) -> SResult<&'static str> {
        if let Expr::Eval(node) = expr
            && node.source.is_none()
            && node.args.is_empty()
            && let Head::Symbol(sym) = &node.head
            && sym.starts_with('$')
        {
            if sym.as_bytes()[1].is_ascii_digit() {
                Err(StreamError::with_expr("reserved variable name", expr))
            } else {
                Ok(*sym)
            }
        } else {
            Err(StreamError::with_expr("expected global variable ($name)", expr))
        }
    }

    fn apply_context<'a>(&self, expr: &'a Expr) -> SResult<Cow<'a, Expr>> {
        match expr {
            Expr::Repl(Subst::History(index)) => match index {
                Some(ix @ 1..) => Ok(Cow::Owned(
                    self.hist
                        .get(ix - 1)
                        .cloned()
                        .map(splice_value)
                        .ok_or(StreamError::with_expr("history item does not exist", expr))?,
                )),
                Some(0) => Err(StreamError::with_expr("invalid history index", expr)),
                None => Ok(Cow::Owned(
                    self.hist
                        .last()
                        .cloned()
                        .map(splice_value)
                        .ok_or(StreamError::with_expr("history is empty", expr))?,
                )),
            },
            Expr::Repl(Subst::Counter) => Ok(Cow::Owned(Expr::new_number(self.counter))),
            Expr::Eval(node) => {
                let mut nnode = Cow::Borrowed(&**node);
                if let Head::Symbol(sym) = &node.head
                    && sym.starts_with('$')
                {
                    match self.vars.get(sym) {
                        Some(Rhs::Value(item)) => {
                            return if node.source.is_none() && node.args.is_empty() {
                                Ok(Cow::Owned(splice_value(item.clone())))
                            } else {
                                Err(StreamError::with_expr("no source or arguments allowed", expr))
                            };
                        },
                        Some(Rhs::Function(closure)) => {
                            nnode.to_mut().head = Head::Closure(closure.clone());
                        },
                        None => return Err(StreamError::with_expr("variable not defined", expr)),
                    }
                } else if let Head::Block(body) = &node.head
                    && let Cow::Owned(new_body) = self.apply_context(body)?
                {
                    nnode.to_mut().head = Head::Block(new_body);
                } else if let Head::Closure(closure) = &node.head
                    && let Cow::Owned(new_body) = self.apply_context(closure.body())?
                {
                    // `${...}` written at the prompt may itself refer to history or globals.
                    nnode.to_mut().head = Head::Closure(Closure::new(new_body, closure.env().clone()));
                }
                if let Some(source) = &node.source
                    && let Cow::Owned(new_source) = self.apply_context(source)?
                {
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
            _ => Ok(Cow::Borrowed(expr)),
        }
    }

    pub fn history(&self) -> &Vec<Item> {
        &self.hist
    }

    /// Sets the tracer receiving the evaluation events of [`process()`](Self::process).
    pub fn set_tracer(&mut self, tracer: Rc<RefCell<impl Tracer + 'static>>) {
        self.tracer = Some(tracer);
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
            _ => panic!("SessionUpdate::unwrap on non-History"),
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
        assert_eq!(sess.process(parse("$a=[1]").unwrap()).unwrap(), SessionUpdate::Globals(vec!["$a"]));
        assert!(sess.process(parse("$a@[2]").unwrap()).is_err());

        let mut sess = Session::new();
        assert_eq!(sess.process(parse("100").unwrap()).unwrap(), SessionUpdate::History(1, &Item::new_number(100)));
        assert_eq!(
            sess.process(parse("% * %1").unwrap()).unwrap(),
            SessionUpdate::History(2, &Item::new_number(10000))
        );
        assert_eq!(
            sess.process(parse("% + %1").unwrap()).unwrap(),
            SessionUpdate::History(3, &Item::new_number(10100))
        );

        let mut sess = Session::new();
        assert_eq!(sess.process(parse("$a={#+#1*#2}").unwrap()).unwrap(), SessionUpdate::Globals(vec!["$a"]));
        assert_eq!(sess.process(parse("5.$a(6,7)").unwrap()).unwrap().unwrap(), &Item::new_number(47));
        assert_eq!(sess.process(parse("{5.$a(6,7)}").unwrap()).unwrap().unwrap(), &Item::new_number(47));
        assert_eq!(sess.process(parse("5.$a@[6,7]").unwrap()).unwrap().unwrap(), &Item::new_number(47));
        assert_eq!(sess.process(parse("5.{#.$a(#1,#2)}(6,7)").unwrap()).unwrap().unwrap(), &Item::new_number(47));
        assert_eq!(
            sess.process(parse("5.{#.$a(#1,#2)}@[6,7]").unwrap())
                .unwrap()
                .unwrap(),
            &Item::new_number(47)
        );

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

    #[test]
    fn test_session_closures() {
        use crate::base::Describe;
        use crate::base::check_roundtrip;

        /// Evaluates `input` in the session, then checks its description and that the description
        /// evaluates to the same thing again at the top level, with no globals or history in sight.
        fn roundtrip(sess: &mut Session, input: &str, desc: &str) {
            check_roundtrip(sess.process(parse(input).unwrap()).unwrap().unwrap(), desc);
        }
        fn eval(sess: &mut Session, input: &str) -> String {
            sess.process(parse(input).unwrap()).unwrap().unwrap().to_string()
        }

        let mut sess = Session::new();
        // A global function is closed over the session environment, wherever it is used.
        sess.process(parse("$g={#+1}").unwrap()).unwrap();
        assert_eq!(eval(&mut sess, "alpha(\"cba\", \"abc\".$g)"), "\"bcd\""); // %1
        roundtrip(&mut sess, "alpha(\"cba\", seq:$g)", "alpha(['c', 'b', 'a'], seq:${#+1})"); // %2
        // ... and stays opaque to the locals of a `with` it is spliced into, even after a round trip.
        sess.process(parse("$a={#.len}").unwrap()).unwrap();
        roundtrip(&mut sess, "[1]:{with(len={1}, range(3).$a)}", "[1]:{with(len={1}, range(3).${#.len})}"); // %3
        assert_eq!(eval(&mut sess, "%"), "[3]"); // %4
        // History and global values: symbol-free ones are spliced as they are, lazy ones as closures.
        assert_eq!(eval(&mut sess, "[[1], [2, 3]]:len"), "[1, 2]"); // %5
        roundtrip(&mut sess, "[1]:{with(len={1}, %5)}", "[1]:{with(len={1}, ${[[1], [2, 3]]:len})}"); // %6
        assert_eq!(eval(&mut sess, "%"), "[[1, 2]]"); // %7
        assert_eq!(eval(&mut sess, "[1, 2]"), "[1, 2]"); // %8
        roundtrip(&mut sess, "seq:{#~%8}", "seq:{#~[1, 2]}"); // %9
        assert_eq!(eval(&mut sess, "seq:{#+1}"), "[2, 3, 4, 5, 6, ...]"); // %10
        roundtrip(&mut sess, "%10~[1]", "seq:{#+1}~[1]"); // %11
        roundtrip(&mut sess, "seq:{#~%10}", "seq:{#~${seq:{#+1}}}"); // %12
        sess.process(parse("$v=[seq]").unwrap()).unwrap();
        roundtrip(&mut sess, "seq:{#~$v}", "seq:{#~${[seq]}}"); // %13
        sess.process(parse("$w=[1, 'a']").unwrap()).unwrap();
        roundtrip(&mut sess, "seq:{#~$w}", "seq:{#~[1, 'a']}"); // %14
        // A value which is in error keeps erroring after the round trip.
        assert!(eval(&mut sess, "seq:{x+#}").contains("<!>")); // %15
        roundtrip(&mut sess, "[1]:{with(x=5, %15)}", "[1]:{with(x=5, ${seq:{x+#}})}"); // %16
        assert!(eval(&mut sess, "%").contains("<!>")); // %17
        // `${...}` written at the prompt may refer to history and globals.
        sess.process(parse("$n=10").unwrap()).unwrap();
        assert_eq!(eval(&mut sess, "${%8+$n}"), "[11, 12]"); // %18
        assert_eq!(eval(&mut sess, "alpha(\"cba\", ${%8:{'a'+#}})"), "['b', 'c']"); // %19
        // A `${...}` right-hand side is a closure already and is stored as is.
        sess.process(parse("$h=${#+1}").unwrap()).unwrap();
        assert_eq!(eval(&mut sess, "1.$h"), "2"); // %20
        assert_eq!(sess.vars().get("$h").unwrap().describe(), "${#+1}");
        assert_eq!(sess.vars().get("$g").unwrap().describe(), "${#+1}");
    }
}

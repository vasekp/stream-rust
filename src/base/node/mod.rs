use crate::base::*;
use crate::keywords::find_keyword;

mod enode;
mod link;
mod head;
mod checks;
mod rnode;
mod dh;

pub(crate) use enode::ENode;
pub use link::Link;
pub(crate) use checks::Checks;
pub use head::{Head, LangItem};
pub(crate) use rnode::*;

#[cfg(test)]
mod tests;

/// A `Node` is a type of [`Expr`] representing a head object along with, optionally, its source
/// and arguments. This is an abstract representation, which may evaluate to a stream or an atomic
/// value, potentially depending on the nature of the source or arguments provided. This evaluation
/// happens in [`Expr::eval()`].
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Node {
    pub head: Head,
    pub source: Option<Box<Expr>>,
    pub args: Vec<Expr>
}

impl Node {
    /// Creates a new `Node`. The `head` may be specified by [`Head`] directly, but also by
    /// anything implementing `Into<String>` ([`Head::Symbol`]), [`LangItem`] ([`Head::Lang`]),
    /// [`Expr`], [`Item`] or [`Node`] (all three for [`Head::Block`]).
    pub fn new(head: impl Into<Head>, source: Option<Expr>, args: Vec<Expr>) -> Node {
        Node{head: head.into(), source: source.map(Box::new), args}
    }

    /// Evaluates this `Node` to an `Item`. This is the point at which it is decided whether it
    /// describes an atomic constant or a stream.
    ///
    /// The evaluation is done by finding the head of the node in a global keyword table.
    /// Locally defined symbols aren't handled here.
    // Note to self: for assignments, this will happen in Session::process. For `with`, this will
    // happen in Expr::apply(Context).
    pub fn eval(self, env: &Env) -> Result<Item, StreamError> {
        env.tracer.borrow_mut().log(tracing::Event::Enter(&self));
        let res = match self.head {
            Head::Symbol(ref sym) | Head::Oper(ref sym) => {
                if let Some(rhs) = env.vars.get(sym) {
                    match rhs {
                        Rhs::Value(item) => {
                            try_with!(self, self.check_no_source()?);
                            try_with!(self, self.check_no_args()?);
                            Ok(item.clone())
                        },
                        Rhs::Function(block, saved_env) => {
                            Expr::Eval(Node {
                                head: block.clone().into(),
                                source: self.source,
                                args: self.args
                            }).eval(saved_env)
                        }
                    }
                } else if let Some(func) = find_keyword(sym) {
                    func(self, env)
                } else {
                    Err(StreamError::new(format!("symbol '{sym}' not found"), self))
                }
            },
            Head::Lang(ref lang) => {
                let ctor = find_keyword(lang.keyword()).expect("all LangItem keywords should exist");
                ctor(self, env)
            },
            Head::Block(blk) => {
                let source = self.source.map(|expr| expr.eval(env)).transpose()?;
                let args = self.args.into_iter()
                    .map(|expr| expr.eval(env))
                    .collect::<Result<_, _>>()?;
                blk.apply(&source, &args)?.eval(env)
            }
        };
        env.tracer.borrow_mut().log(tracing::Event::Leave(&res));
        res
    }

    pub(crate) fn eval_all(self, env: &Env) -> Result<ENode, StreamError> {
        let source = match self.source {
            Some(source) => Some(source.eval(env)?),
            None => None
        };
        let args = self.args.into_iter()
            .map(|x| x.eval(env))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(ENode{head: self.head, source, args})
    }

    pub(crate) fn eval_source(self, env: &Env) -> Result<RNodeS<Item, Expr>, StreamError> {
        match self.source {
            Some(source) => Ok(RNodeS {
                head: self.head,
                source: source.eval(env)?,
                args: self.args.into()
            }),
            None => Err(StreamError::new("source required", self))
        }
    }

    /*pub(crate) fn eval_args(mut self, env: &Env) -> Result<Node, StreamError> {
        self.args = self.args.into_iter()
            .map(|x| x.eval(env).map(Expr::from))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(self)
    }*/

    pub(crate) fn eval_nth_arg(mut self, ix: usize, env: &Env) -> Result<Node, StreamError> {
        if ix >= self.args.len() {
            return Err(StreamError::new("not enough arguments", self));
        }
        let arg = self.args.remove(ix);
        let arg = arg.eval(env)?;
        self.args.insert(ix, arg.into());
        Ok(self)
    }

    pub(in crate::base) fn apply(self, source: &Option<Item>, args: &Vec<Item>) -> Result<Node, StreamError> {
        Ok(Node {
            head: self.head,
            source: match self.source {
                None => None,
                Some(boxed) => Some(Box::new(boxed.apply(source, args)?))
            },
            args: self.args.into_iter()
                .map(|expr| expr.apply(source, args))
                .collect::<Result<Vec<_>, _>>()?
        })
    }

    pub(crate) fn with_source(mut self, source: Expr) -> Result<Node, StreamError> {
        if self.source.is_some() {
            Err(StreamError::new("already has source", self))
        } else {
            self.source = Some(Box::new(source));
            Ok(self)
        }
    }

    pub(crate) fn with_args(mut self, args: Vec<Expr>) -> Result<Node, StreamError> {
        if !self.args.is_empty() {
            Err(StreamError::new("already has arguments", self))
        } else {
            self.args = args;
            Ok(self)
        }
    }

    #[allow(unused)]
    pub(crate) fn resolve(self) -> RNode<Expr> {
        match self.source {
            Some(source) => RNode::Source(RNodeS { head: self.head, source: *source, args: self.args.into() }),
            None => RNode::NoSource(RNodeNS { head: self.head, args: self.args.into() }),
        }
    }

    #[allow(unused)]
    pub(crate) fn resolve_source(self) -> Result<RNodeS<Expr>, StreamError> {
        match self.source {
            Some(source) => Ok(RNodeS { head: self.head, source: *source, args: self.args.into() }),
            None => Err(StreamError::new("source required", self))
        }
    }

    #[allow(unused)]
    pub(crate) fn resolve_no_source(self) -> Result<RNodeNS<Expr>, StreamError> {
        match self.source {
            Some(_) => Err(StreamError::new("no source accepted", self)),
            None => Ok(RNodeNS { head: self.head, args: self.args.into() })
        }
    }

    pub(crate) fn describe_helper<T, U>(
        head: &Head,
        source: Option<&T>,
        args: impl IntoIterator<Item = U>,
        prec: u32,
        env: &Env)
    -> String
        where T: Describe, U: Describe
    {
        dh::describe_helper(head, source, args.into_iter(), prec, env)
    }

    pub(crate) fn describe_with_env<T, U>(
        env_inner: &Env,
        head: &Head,
        source: Option<&T>,
        args: impl IntoIterator<Item = U>,
        prec: u32,
        env_outer: &Env)
    -> String
        where T: Describe, U: Describe
    {
        env_inner.wrap_describe(|prec, env|
            dh::describe_helper(head, source, args.into_iter(), prec, env), prec, env_outer)
    }

    pub(crate) fn describe_with_alpha<T, U>(
        alpha: &Rc<Alphabet>,
        head: &Head,
        source: Option<&T>,
        args: impl IntoIterator<Item = U>,
        prec: u32,
        env: &Env)
    -> String
        where T: Describe, U: Describe
    {
        alpha.wrap_describe(|prec, env|
            dh::describe_helper(head, source, args.into_iter(), prec, env), prec, env)
    }
}

impl Describe for Node {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        dh::describe_helper(&self.head, self.source.as_deref(), self.args.iter(), prec, env)
    }
}

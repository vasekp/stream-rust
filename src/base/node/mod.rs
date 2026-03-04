use crate::base::*;
use crate::symbols::Symbols;

mod enode;
mod link;
mod head;
mod checks;
mod rnode;
mod db;

pub(crate) use enode::ENode;
pub use link::Link;
pub(crate) use checks::Checks;
pub use head::{Head, LangItem};
pub(crate) use rnode::*;
pub(crate) use db::DescribeBuilder;

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
    pub source: Option<Expr>,
    pub args: Vec<Expr>
}

impl Node {
    /// Creates a new `Node`. The `head` may be specified by [`Head`] directly, but also by
    /// anything implementing `Into<String>` ([`Head::Symbol`]), [`LangItem`] ([`Head::Lang`]),
    /// [`Expr`], [`Item`] or [`Node`] (all three for [`Head::Block`]).
    pub fn new(head: impl Into<Head>, source: Option<Expr>, args: Vec<Expr>) -> Node {
        Node{head: head.into(), source, args}
    }

    /// Evaluates this `Node` to an `Item`. This is the point at which it is decided whether it
    /// describes an atomic constant or a stream.
    ///
    /// The evaluation is done by finding the head of the node in a global symbol table.
    /// Locally defined symbols aren't handled here.
    // Note to self: for assignments, this will happen in Session::process. For `with`, this will
    // happen in Expr::apply(Context).
    pub fn eval(&self, env: &Env) -> Result<Item, StreamError> {
        env.tracer.borrow_mut().log(tracing::Event::Enter(self));
        let res = (|| {
            match &self.head {
                Head::Symbol(sym) | Head::Oper(sym) => {
                    if let Some(ctor) = Symbols::find_ctor(sym) {
                        ctor(self, env)
                    } else {
                        Err(StreamError::new0(format!("symbol '{sym}' not found")))
                    }
                },
                Head::Lang(lang) => {
                    let ctor = Symbols::find_ctor(lang.symbol()).expect("all LangItem symbols should exist");
                    ctor(self, env)
                },
                Head::Block(blk) => {
                    let source = self.source.as_ref().map(|expr| expr.eval(env)).transpose()?;
                    let args = self.args.iter()
                        .map(|expr| expr.eval(env))
                        .collect::<Result<_, _>>()?;
                    blk.apply(&source, &args)?.eval(env)
                },
            }
        })();
        env.tracer.borrow_mut().log(tracing::Event::Leave(&res));
        res
    }

    pub(crate) fn eval_all(&self, env: &Env) -> Result<ENode, StreamError> {
        let source = match &self.source {
            Some(source) => Some(source.eval(env)?),
            None => None
        };
        let args = self.args.iter()
            .map(|x| x.eval(env))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(ENode{head: self.head.clone(), source, args})
    }

    pub(crate) fn eval_source(&self, env: &Env) -> Result<RNodeS<Item, Expr>, StreamError> {
        match &self.source {
            Some(source) => Ok(RNodeS {
                head: self.head.clone(),
                source: source.eval(env)?,
                args: self.args.clone().into(),
            }),
            None => Err(StreamError::new0("source required"))
        }
    }

    /*pub(crate) fn eval_args(mut self, env: &Env) -> Result<Node, StreamError> {
        self.args = self.args.into_iter()
            .map(|x| x.eval(env).map(Expr::from))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(self)
    }*/

    pub(crate) fn eval_nth_arg(&self, ix: usize, env: &Env) -> Result<Node, StreamError> {
        if ix >= self.args.len() {
            return Err(StreamError::new0("not enough arguments"));
        }
        let mut args = self.args.clone();
        let arg = args.remove(ix);
        let arg = arg.eval(env)?;
        args.insert(ix, arg.into());
        Ok(Node{head: self.head.clone(), source: self.source.clone(), args})
    }

    pub(in crate::base) fn apply(&self, source: &Option<Item>, args: &Vec<Item>) -> Result<Node, StreamError> {
        Ok(Node {
            head: self.head.clone(),
            source: match &self.source {
                None => None,
                Some(src) => Some(src.apply(source, args)?)
            },
            args: self.args.iter()
                .map(|expr| expr.apply(source, args))
                .collect::<Result<Vec<_>, _>>()?
        })
    }

    pub(crate) fn with_source(&self, source: Expr) -> Result<Node, StreamError> {
        if self.source.is_some() {
            Err(StreamError::new0("already has source"))
        } else {
            Ok(Node{head: self.head.clone(), source: Some(source), args: self.args.clone()})
        }
    }

    pub(crate) fn with_args(&self, args: Vec<Expr>) -> Result<Node, StreamError> {
        if !self.args.is_empty() {
            Err(StreamError::new0("already has arguments"))
        } else {
            Ok(Node{head: self.head.clone(), source: self.source.clone(), args})
        }
    }

    #[allow(unused)]
    pub(crate) fn resolve(&self) -> RNode<Expr> {
        match &self.source {
            Some(source) => RNode::Source(RNodeS {
                head: self.head.clone(),
                source: source.clone(),
                args: self.args.clone().into()
            }),
            None => RNode::NoSource(RNodeNS {
                head: self.head.clone(),
                args: self.args.clone().into()
            }),
        }
    }

    #[allow(unused)]
    pub(crate) fn resolve_source(&self) -> Result<RNodeS<Expr>, StreamError> {
        match &self.source {
            Some(source) => Ok(RNodeS {
                head: self.head.clone(),
                source: source.clone(),
                args: self.args.clone().into()
            }),
            None => Err(StreamError::new0("source required"))
        }
    }

    #[allow(unused)]
    pub(crate) fn resolve_no_source(&self) -> Result<RNodeNS<Expr>, StreamError> {
        match self.source {
            Some(_) => Err(StreamError::new0("no source accepted")),
            None => Ok(RNodeNS {
                head: self.head.clone(),
                args: self.args.clone().into()
            })
        }
    }
}

impl Describe for Node {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source_opt(&self.source)
            .push_args(&self.args)
            .finish(prec)
    }
}

use crate::base::*;
use crate::symbols::Symbols;

mod link;
mod head;
mod db;

pub use link::Link;
pub use head::{Head, LangItem};
pub(crate) use db::DescribeBuilder;

#[cfg(test)]
mod tests;

/// A `Node` is a type of [`Expr`] representing a head object along with, optionally, its source
/// and arguments. This is an abstract representation, which may evaluate to a stream or an atomic
/// value, potentially depending on the nature of the source or arguments provided. This evaluation
/// happens in [`Expr::eval()`].
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Node<I = Expr> {
    pub head: Head,
    pub source: Option<I>,
    pub args: Vec<I>
}

impl Node<Expr> {
    /// Evaluates this `Node` to an `Item`. This is the point at which it is decided whether it
    /// describes an atomic constant or a stream.
    ///
    /// The evaluation is done by finding the head of the node in a global symbol table.
    /// Locally defined symbols aren't handled here.
    // Note to self: for assignments, this will happen in Session::process. For `with`, this will
    // happen in Expr::apply(Context).
    pub fn eval(&self, env: &Env) -> SResult<Item> {
        env.tracer.borrow_mut().log(tracing::Event::Enter(self));
        let res = (|| {
            match &self.head {
                Head::Symbol(sym) | Head::Oper(sym) => {
                    if let Some(ctor) = Symbols::find_ctor(sym) {
                        ctor(self, env)
                    } else {
                        Err(format!("symbol '{sym}' not found").into())
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
                        .collect::<SResult<_>>()?;
                    blk.apply(&source, &args)?.eval(env)
                },
            }
        })();
        env.tracer.borrow_mut().log(tracing::Event::Leave(&res));
        res
    }

    pub(crate) fn eval_all(&self, env: &Env) -> SResult<Node<Item>> {
        let source = match &self.source {
            Some(source) => Some(source.eval(env)?),
            None => None
        };
        let args = self.args.iter()
            .map(|x| x.eval(env))
            .collect::<SResult<Vec<_>>>()?;
        Ok(Node{head: self.head.clone(), source, args})
    }

    pub(in crate::base) fn apply(&self, source: &Option<Item>, args: &Vec<Item>) -> SResult<Node> {
        Ok(Node {
            head: self.head.clone(),
            source: match &self.source {
                None => None,
                Some(src) => Some(src.apply(source, args)?)
            },
            args: self.args.iter()
                .map(|expr| expr.apply(source, args))
                .collect::<SResult<Vec<_>>>()?
        })
    }

    pub(crate) fn with_source(&self, source: Expr) -> SResult<Self> {
        if self.source.is_some() {
            Err(StreamError::with_expr("already has source", self))
        } else {
            Ok(Node{head: self.head.clone(), source: Some(source), args: self.args.clone()})
        }
    }

    pub(crate) fn with_args(&self, args: Vec<Expr>) -> SResult<Self> {
        if !self.args.is_empty() {
            Err(StreamError::with_expr("already has arguments", self))
        } else {
            Ok(Node{head: self.head.clone(), source: self.source.clone(), args})
        }
    }
}

impl<I: Clone> Node<I> {
    /// Creates a new `Node`. The `head` may be specified by [`Head`] directly, but also by
    /// anything implementing `Into<String>` ([`Head::Symbol`]), [`LangItem`] ([`Head::Lang`]),
    /// [`Expr`], [`Item`] or [`Node`] (all three for [`Head::Block`]).
    pub fn new(head: impl Into<Head>, source: Option<I>, args: Vec<I>) -> Self {
        Node{head: head.into(), source, args}
    }

    pub(crate) fn check_source(&self) -> SResult<()> {
        match &self.source {
            Some(_) => Ok(()),
            None => Err(StreamError::usage(&self.head))
        }
    }

    pub(crate) fn check_no_source(&self) -> SResult<()> {
        match &self.source {
            Some(_) => Err(StreamError::usage(&self.head)),
            None => Ok(())
        }
    }

    pub(crate) fn source_checked(&self) -> SResult<&I> {
        self.source.as_ref().ok_or_else(|| StreamError::usage(&self.head))
    }

    pub(crate) fn check_no_args(&self) -> SResult<()> {
        if !self.args.is_empty() {
            Err(StreamError::usage(&self.head))
        } else {
            Ok(())
        }
    }

    pub(crate) fn check_args_nonempty(&self) -> SResult<()> {
        if self.args.is_empty() {
            Err(StreamError::usage(&self.head))
        } else {
            Ok(())
        }
    }

    pub(crate) fn first_arg_checked(&self) -> SResult<&I> {
        self.args.first().ok_or(StreamError::usage(&self.head))
    }
}

impl<I: Describe> Describe for Node<I> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source_opt(&self.source)
            .push_args(&self.args)
            .finish(prec)
    }
}

impl From<Node<Item>> for Node<Expr> {
    fn from(node: Node<Item>) -> Node {
        Node {
            head: node.head,
            source: node.source.map(Expr::from),
            args: node.args.into_iter().map(Expr::from).collect()
        }
    }
}

impl From<&Node<Item>> for Node<Expr> {
    fn from(node: &Node<Item>) -> Node {
        node.clone().into()
    }
}

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
/// happens in [`Node::eval()`].
///
/// The type parameter is the type of the source and arguments: `Node<Expr>` (the default) is the
/// unevaluated form produced by the parser, `Node<Item>` is what the operators receive after
/// `eval_all()`.
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Node<I = Expr> {
    pub head: Head,
    pub source: Option<I>,
    pub args: Vec<I>,
}

impl Node<Expr> {
    /// Evaluates this `Node` to an `Item`. This is the point at which it is decided whether it
    /// describes an atomic constant or a stream.
    ///
    /// A [`Head::Symbol`] or [`Head::Oper`] is looked up in the global symbol table and its
    /// constructor is called with this node and `env`; a [`Head::Lang`] is handled the same way
    /// through its reserved symbol. For a [`Head::Block`], the source and arguments are evaluated
    /// first, substituted for `#`, `#1`, ... in the body, and the body is evaluated in turn (in
    /// the default environment if `reset_env` is set).
    ///
    /// Variables are not handled here. Local ones are substituted into the body by `with` before
    /// it is evaluated; session globals and history references by
    /// [`Session::process()`](crate::session::Session::process) before the expression is passed
    /// to this function. Entry and exit are reported to the installed tracer, if any (see
    /// [`tracing::install()`]).
    pub fn eval(&self, env: &Env) -> SResult<Item> {
        tracing::log(tracing::Event::Enter(self));
        let res = (|| match &self.head {
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
            Head::Block { body, reset_env } => {
                let env = if *reset_env { Env::default() } else { env.clone() };
                let source = self.source.as_ref().map(|expr| expr.eval(&env)).transpose()?;
                let args = self.args.iter().map(|expr| expr.eval(&env)).collect::<SResult<_>>()?;
                body.apply(&source, &args)?.eval(&env)
            },
        })();
        tracing::log(tracing::Event::Leave(&res));
        res
    }

    pub(crate) fn eval_all(&self, env: &Env) -> SResult<Node<Item>> {
        let source = match &self.source {
            Some(source) => Some(source.eval(env)?),
            None => None,
        };
        let args = self.args.iter().map(|x| x.eval(env)).collect::<SResult<Vec<_>>>()?;
        Ok(Node { head: self.head.clone(), source, args })
    }

    pub(in crate::base) fn apply(&self, source: &Option<Item>, args: &Vec<Item>) -> SResult<Node> {
        Ok(Node {
            head: self.head.clone(),
            source: match &self.source {
                None => None,
                Some(src) => Some(src.apply(source, args)?),
            },
            args: self
                .args
                .iter()
                .map(|expr| expr.apply(source, args))
                .collect::<SResult<Vec<_>>>()?,
        })
    }

    pub(crate) fn with_source(&self, source: Expr) -> SResult<Self> {
        if self.source.is_some() {
            Err(StreamError::with_expr("already has source", self))
        } else {
            Ok(Node { head: self.head.clone(), source: Some(source), args: self.args.clone() })
        }
    }

    pub(crate) fn with_args(&self, args: Vec<Expr>) -> SResult<Self> {
        if !self.args.is_empty() {
            Err(StreamError::with_expr("already has arguments", self))
        } else {
            Ok(Node { head: self.head.clone(), source: self.source.clone(), args })
        }
    }
}

impl<I: Clone> Node<I> {
    /// Creates a new `Node`. The `head` may be specified by [`Head`] directly, but also by a
    /// `&str` ([`Head::Symbol`]), [`LangItem`] ([`Head::Lang`]), [`Expr`], [`Item`] or [`Node`]
    /// (all three for [`Head::Block`]).
    pub fn new(head: impl Into<Head>, source: Option<I>, args: Vec<I>) -> Self {
        Node { head: head.into(), source, args }
    }

    pub(crate) fn check_source(&self) -> SResult<()> {
        match &self.source {
            Some(_) => Ok(()),
            None => Err(StreamError::usage(&self.head)),
        }
    }

    pub(crate) fn check_no_source(&self) -> SResult<()> {
        match &self.source {
            Some(_) => Err(StreamError::usage(&self.head)),
            None => Ok(()),
        }
    }

    pub(crate) fn source_checked(&self) -> SResult<&I> {
        self.source.as_ref().ok_or_else(|| StreamError::usage(&self.head))
    }

    pub(crate) fn check_no_args(&self) -> SResult<()> {
        if !self.args.is_empty() { Err(StreamError::usage(&self.head)) } else { Ok(()) }
    }

    pub(crate) fn check_args_nonempty(&self) -> SResult<()> {
        if self.args.is_empty() { Err(StreamError::usage(&self.head)) } else { Ok(()) }
    }

    pub(crate) fn first_arg_checked(&self) -> SResult<&I> {
        self.args.first().ok_or(StreamError::usage(&self.head))
    }

    pub(crate) fn only_arg_checked(&self) -> SResult<&I> {
        match &self.args[..] {
            [arg] => Ok(arg),
            _ => Err(StreamError::usage(&self.head)),
        }
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
            args: node.args.into_iter().map(Expr::from).collect(),
        }
    }
}

impl From<&Node<Item>> for Node<Expr> {
    fn from(node: &Node<Item>) -> Node {
        node.clone().into()
    }
}

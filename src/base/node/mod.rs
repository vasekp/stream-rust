use crate::base::*;
use crate::keywords::find_keyword;

mod enode;
mod link;
mod head;
mod checks;
mod rnode;

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
#[derive(Debug, PartialEq, Clone)]
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
    pub fn eval(self, env: &Rc<Env>) -> Result<Item, StreamError> {
        match self.head {
            Head::Symbol(ref sym) | Head::Oper(ref sym) => match find_keyword(sym) {
                Ok(func) => func(self, env),
                Err(e) => Err(StreamError::new(e, self))
            },
            Head::Lang(ref lang) => {
                let ctor = find_keyword(lang.keyword()).expect("all LangItem keywords should exist");
                ctor(self, env)
            },
            Head::Block(blk) => blk.apply(&self.source, &self.args)?.eval_env(env),
            Head::Args(_) => Node::eval_at(self, env),
            Head::Repl(_, _) => Err(StreamError::new("out of context", self))
        }
    }

    pub(crate) fn eval_all(self, env: &Rc<Env>) -> Result<ENode, StreamError> {
        let source = match self.source {
            Some(source) => Some(source.eval_env(env)?),
            None => None
        };
        let args = self.args.into_iter()
            .map(|x| x.eval_env(env))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(ENode{head: self.head, source, args})
    }

    pub(crate) fn eval_source(mut self, env: &Rc<Env>) -> Result<Node, StreamError> {
        if let Some(source) = self.source.take() {
            self.source = Some(Box::new(source.eval_env(env)?.into()));
        }
        Ok(self)
    }

    /*pub(crate) fn eval_args(mut self, env: &Rc<Env>) -> Result<Node, StreamError> {
        self.args = self.args.into_iter()
            .map(|x| x.eval_env(env).map(Expr::from))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(self)
    }*/

    fn eval_at(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        let node = node.eval_all(env)?;
        debug_assert!(node.args.len() == 1);
        let src_stream = try_with!(node, node.args[0].as_stream()?);
        if src_stream.length() == Length::Infinite {
            return Err(StreamError::new("stream is infinite", node));
        }
        let Head::Args(head) = node.head else { unreachable!() };
        let expr = Expr::Eval(Node{
            head: *head,
            source: node.source.map(|item| Box::new(item.into())),
            args: src_stream.iter()
                .map(|res| res.map(Expr::from))
                .collect::<Result<Vec<_>, _>>()?
        });
        expr.eval_env(env)
    }

    pub(crate) fn apply(self, source: &Option<Box<Expr>>, args: &Vec<Expr>) -> Result<Node, StreamError> {
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

    #[allow(unused)]
    pub(crate) fn with_args(mut self, args: Vec<Expr>) -> Result<Node, StreamError> {
        if !self.args.is_empty() {
            Err(StreamError::new("already has arguments", self))
        } else {
            self.args = args;
            Ok(self)
        }
    }

    pub(crate) fn describe_helper<'a, T, U>(
        head: &Head,
        source: Option<&T>,
        args: impl IntoIterator<Item = &'a U>,
        prec: u32)
    -> String
        where T: Describe, U: Describe + 'a
    {
        let mut ret = String::new();
        if let Some(source) = source {
            ret += &source.describe_prec(u32::MAX);
            match head {
                Head::Lang(LangItem::Map) => ret.push(':'),
                Head::Lang(LangItem::Part) => (),
                _ => ret.push('.')
            }
        }
        ret += &head.describe();
        let args = args.into_iter();
        if let Head::Oper(op) = head {
            let (nprec, multi) = op_rules(op);
            let parens = (nprec < prec) || (nprec == prec && !multi);
            if parens {
                ret.push('(');
            }
            let mut it = args.map(|arg| arg.describe_prec(nprec));
            let first = it.next().expect("Head::Oper should have at least one arg");
            // if len == 1, print {op}{arg}, otherwise {arg}{op}{arg}...
            match it.next() {
                Some(string) => {
                    ret += &first;
                    ret += op;
                    ret += &string;
                },
                None => {
                    ret += op;
                    ret += &first;
                }
            }
            for string in it {
                ret += op;
                ret += &string;
            }
            if parens {
                ret.push(')');
            }
        } else {
            let mut it = args.map(|arg| arg.describe_prec(0));
            match it.next() {
                Some(first) => {
                    match head {
                        Head::Lang(LangItem::Part | LangItem::List) => ret.push('['),
                        Head::Lang(LangItem::Map) => (),
                        _ => ret.push('(')
                    }
                    ret += &first;
                    for s in it {
                        ret += ", ";
                        ret += &s
                    }
                    match head {
                        Head::Lang(LangItem::Part | LangItem::List) => ret.push(']'),
                        Head::Lang(LangItem::Map) => (),
                        _ => ret.push(')')
                    };
                },
                None => if head == &Head::Lang(LangItem::List) {
                    ret += "[]";
                }
            }
        }
        ret
    }
}

impl Describe for Node {
    fn describe_prec(&self, prec: u32) -> String {
        Node::describe_helper(&self.head, self.source.as_deref(), &self.args, prec)
    }
}

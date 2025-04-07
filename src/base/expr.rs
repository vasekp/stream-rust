use crate::base::*;

use std::rc::Rc;

/// Any Stream language expression. This may be either a directly accessible [`Item`] (including
/// e.g. literal expressions) or a [`Node`], which becomes [`Item`] on evaluation.
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Imm(Item),
    Eval(Node)
}

impl Expr {
    pub fn new_number(value: impl Into<Number>) -> Expr {
        Item::new_number(value).into()
    }

    pub fn new_bool(value: bool) -> Expr {
        Item::new_bool(value).into()
    }

    pub fn new_char(value: impl Into<Char>) -> Expr {
        Item::new_char(value).into()
    }

    pub fn new_stream(value: impl Stream + 'static) -> Expr {
        Item::new_stream(value).into()
    }

    pub fn new_string(value: impl Into<String>) -> Expr {
        Item::new_string(value).into()
    }

    pub fn new_node(head: impl Into<Head>, args: Vec<Expr>) -> Expr {
        Expr::Eval(Node{head: head.into(), source: None, args})
    }

    /// Creates an operator expression. Operands are provided as `args`.
    pub fn new_op(op: impl Into<String>, args: Vec<Expr>) -> Expr {
        Expr::Eval(Node{head: Head::Oper(op.into()), source: None, args})
    }

    /// Creates a special expression `#(n)` or `$(n)`.
    pub fn new_repl(chr: char, ix: Option<usize>) -> Expr {
        Expr::Eval(Node{
            head: Head::Repl(chr, ix),
            source: None,
            args: vec![]
        })
    }

    /// Makes the output of this expression an input to a [`Link`].
    pub fn chain(self, next: Link) -> Expr {
        Expr::Eval(Node{
            head: next.head,
            source: Some(Box::new(self)),
            args: next.args
        })
    }

    pub fn as_item(&self) -> Result<&Item, BaseError> {
        match self {
            Expr::Imm(ref item) => Ok(item),
            Expr::Eval(node) => Err(format!("expected value, found {:?}", node).into()),
        }
    }

    pub fn as_item_mut(&mut self) -> Result<&mut Item, BaseError> {
        match self {
            Expr::Imm(ref mut item) => Ok(item),
            Expr::Eval(node) => Err(format!("expected value, found {:?}", node).into()),
        }
    }

    pub fn to_item(&self) -> Result<Item, BaseError> {
        match self {
            Expr::Imm(item) => Ok(item.clone()),
            Expr::Eval(node) => Err(format!("expected value, found {:?}", node).into()),
        }
    }

    pub fn into_item(self) -> Result<Item, BaseError> {
        match self {
            Expr::Imm(item) => Ok(item),
            Expr::Eval(node) => Err(format!("expected value, found {:?}", node).into()),
        }
    }

    pub fn to_node(&self) -> Result<Node, BaseError> {
        match self {
            Expr::Eval(node) => Ok(node.to_owned()),
            Expr::Imm(imm) => Err(format!("expected node, found {:?}", imm).into()),
        }
    }

    pub(crate) fn apply(self, source: &Option<Box<Expr>>, args: &Vec<Expr>) -> Result<Expr, StreamError> {
        match self {
            Expr::Imm(_) => Ok(self),
            Expr::Eval(node) => match node.head {
                Head::Repl('#', None) => source.as_ref()
                        .ok_or(StreamError::new("no source provided", node))
                        .map(|boxed| (**boxed).clone()),
                Head::Repl('#', Some(ix)) => args.get(ix - 1)
                    .ok_or(StreamError::new("no such input", node))
                    .cloned(),
                _ => Ok(Expr::Eval(node.apply(source, args)?))
            }
        }
    }

    /// Evaluates this `Expr` in a default environment.
    pub fn eval(self) -> Result<Item, StreamError> {
        self.eval_env(&Default::default())
    }

    /// Evaluates this `Expr`. If it already describes an `Item`, returns that, otherwise calls
    /// `Node::eval_env()`.
    pub fn eval_env(self, env: &Rc<Env>) -> Result<Item, StreamError> {
        match self {
            Expr::Imm(item) => Ok(item),
            Expr::Eval(node) => node.eval(env)
        }
    }
}

impl Default for Expr {
    fn default() -> Expr {
        Expr::Imm(Default::default())
    }
}

impl From<Item> for Expr {
    fn from(item: Item) -> Expr {
        Expr::Imm(item)
    }
}

impl<T> From<T> for Expr where T: Into<Node> {
    fn from(item: T) -> Expr {
        Expr::Eval(item.into())
    }
}

impl Describe for Expr {
    fn describe(&self) -> String {
        match self {
            Expr::Imm(item) => item.describe(),
            Expr::Eval(node) => node.describe()
        }
    }
}

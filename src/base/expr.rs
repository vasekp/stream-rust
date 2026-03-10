use crate::base::*;
use crate::interner::intern;

/// Any Stream language expression. This may be either a directly accessible [`Item`] (including
/// e.g. literal expressions) or a [`Node`], which becomes [`Item`] on evaluation.
// TODO docstring
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Expr {
    Imm(Item),
    Eval(Rc<Node>),
    Repl(Subst),
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

    pub fn new_string(value: &str) -> Expr {
        Item::new_string(LiteralString::from(value)).into()
    }

    pub fn new_node(head: impl Into<Head>, source: Option<Expr>, args: Vec<Expr>) -> Expr {
        Expr::Eval(Rc::new(Node{head: head.into(), source, args}))
    }

    /// Creates an operator expression. Operands are provided as `args`.
    pub fn new_op(op: &str, args: Vec<Expr>) -> Expr {
        Expr::Eval(Rc::new(Node{head: Head::Oper(intern(op)), source: None, args}))
    }

    /// Makes the output of this expression an input to a [`Link`].
    pub fn chain(self, next: Link) -> Expr {
        Expr::Eval(Rc::new(Node{
            head: next.head,
            source: Some(self),
            args: next.args
        }))
    }

    pub(in crate::base) fn apply(&self, source: &Option<Item>, args: &Vec<Item>) -> Result<Expr, StreamError> {
        match self {
            Expr::Eval(node) => Ok(Expr::Eval(Rc::new(node.apply(source, args)?))),
            Expr::Repl(Subst::Input(index)) =>
                match index {
                    None => source.as_ref()
                        .ok_or(StreamError::with_expr("no source provided", self))
                        .map(|item| item.clone().into()),
                    Some(ix) => args.get(ix - 1)
                        .ok_or(StreamError::with_expr("no such input", self))
                        .map(|item| item.clone().into()),
                },
            Expr::Repl(Subst::InputList) =>
                Ok(Expr::new_stream(List::from(args.to_owned()))),
            _ => Ok(self.clone())
        }
    }

    /// Evaluates this `Expr` in a default environment.
    pub fn eval_default(&self) -> Result<Item, StreamError> {
        self.eval(&Default::default())
    }

    /// Evaluates this `Expr`. If it already describes an `Item`, returns that, otherwise calls
    /// `Node::eval()`.
    pub fn eval(&self, env: &Env) -> Result<Item, StreamError> {
        match self {
            Expr::Imm(item) => Ok(item.clone()),
            Expr::Eval(node) => node.eval(env),
            Expr::Repl(_) => Err(StreamError::with_expr("out of context", self))
        }.map_err(|expr| expr.wrap(self))
    }

    pub fn replace(&self, func: &impl Fn(&Expr) -> Result<std::borrow::Cow<'_, Expr>, StreamError>) -> Result<std::borrow::Cow<'_, Expr>, StreamError> {
        use std::borrow::Cow;
        if let Cow::Owned(expr) = func(self)? {
            Ok(Cow::Owned(expr))
        } else if let Expr::Eval(node) = self {
            let new_head = if let Head::Block(expr) = &node.head
                && let Cow::Owned(expr) = expr.replace(func)? {
                    Cow::Owned(Head::Block(expr))
            } else {
                Cow::Borrowed(&node.head)
            };
            let new_source = if let Some(source) = &node.source
                && let Cow::Owned(expr) = source.replace(func)? {
                Cow::Owned(Some(expr))
            } else {
                Cow::Borrowed(&node.source)
            };
            let new_args = node.args.iter()
                .map(|expr| expr.replace(func))
                .collect::<Result<Vec<_>, _>>()?;
            let res = if matches!(new_head, Cow::Owned(_))
                || matches!(new_source, Cow::Owned(_))
                || new_args.iter().any(|cow| matches!(cow, Cow::Owned(_))) {
                    Cow::Owned(Expr::Eval(Rc::new(Node {
                        head: new_head.into_owned(),
                        source: new_source.into_owned(),
                        args: new_args.into_iter().map(Cow::into_owned).collect(),
                    })))
            } else {
                Cow::Borrowed(self)
            };
            Ok(res)
        } else {
            Ok(Cow::Borrowed(self))
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

impl From<&Item> for Expr {
    fn from(item: &Item) -> Expr {
        Expr::Imm(item.clone())
    }
}

impl From<&Rc<Node>> for Expr {
    fn from(node: &Rc<Node>) -> Expr {
        Expr::Eval(Rc::clone(node))
    }
}

impl<T> From<T> for Expr where T: Into<Node> {
    fn from(item: T) -> Expr {
        Expr::Eval(Rc::new(item.into()))
    }
}

impl Describe for Expr {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        match self {
            Expr::Imm(item) => item.describe_inner(prec, env),
            Expr::Eval(node) => node.describe_inner(prec, env),
            Expr::Repl(subs) => subs.to_string(),
        }
    }
}


/// The expression type for placeholder symbols like `#1`, `%`, etc.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Subst {
    /// Input slot (`#`, `#ix`)
    Input(Option<usize>),
    /// Argument list slot (`##`)
    InputList,
    /// History item (`%`, `%ix`)
    History(Option<usize>),
    /// Input counter (`$#`)
    Counter,
}

#[allow(clippy::to_string_trait_impl)]
impl ToString for Subst {
    fn to_string(&self) -> String {
        match self {
            Subst::Input(Some(ix)) => format!("#{ix}"),
            Subst::Input(None) => "#".into(),
            Subst::InputList => "##".into(),
            Subst::History(Some(ix)) => format!("%{ix}"),
            Subst::History(None) => "%".into(),
            Subst::Counter => "$#".into(),
        }
    }
}

#[allow(unused_macros)]
macro_rules! eval {
    ($input:expr) => { crate::parser::parse($input).unwrap().eval_default().unwrap() }
}

#[allow(unused)]
pub(crate) use eval;

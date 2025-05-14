use crate::base::*;

/// Any Stream language expression. This may be either a directly accessible [`Item`] (including
/// e.g. literal expressions) or a [`Node`], which becomes [`Item`] on evaluation.
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Imm(Item),
    Eval(Node),
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

    /// Makes the output of this expression an input to a [`Link`].
    pub fn chain(self, next: Link) -> Expr {
        Expr::Eval(Node{
            head: next.head,
            source: Some(Box::new(self)),
            args: next.args
        })
    }

    pub(in crate::base) fn apply(self, source: &Option<Item>, args: &Vec<Item>) -> Result<Expr, StreamError> {
        match self {
            Expr::Eval(node) => Ok(Expr::Eval(node.apply(source, args)?)),
            Expr::Repl(Subst::Input(index)) =>
                match index {
                    None => source.as_ref()
                        .ok_or(StreamError::new("no source provided", self))
                        .map(|item| item.clone().into()),
                    Some(ix) => args.get(ix - 1)
                        .ok_or(StreamError::new("no such input", self))
                        .map(|item| item.clone().into()),
                },
            Expr::Repl(Subst::InputList) =>
                Ok(Expr::new_stream(List::from(args.to_owned()))),
            _ => Ok(self)
        }
    }

    /// Evaluates this `Expr` in a default environment.
    pub fn eval_default(self) -> Result<Item, StreamError> {
        self.eval(&Default::default())
    }

    /// Evaluates this `Expr`. If it already describes an `Item`, returns that, otherwise calls
    /// `Node::eval()`.
    pub fn eval(self, env: &Rc<Env>) -> Result<Item, StreamError> {
        match self {
            Expr::Imm(item) => Ok(item),
            Expr::Eval(node) => node.eval(env),
            Expr::Repl(_) => Err(StreamError::new("out of context", self))
        }
    }

    pub fn replace(self, func: &impl Fn(Expr) -> Result<Expr, StreamError>) -> Result<Expr, StreamError> {
        match func(self)? {
            Expr::Eval(mut node) => {
                if let Head::Block(ref mut expr) = &mut node.head {
                    *expr = Box::new(std::mem::take(expr).replace(func)?);
                }
                if let Some(expr) = node.source.take() {
                    node.source = Some(Box::new((*expr).replace(func)?));
                }
                node.args = std::mem::take(&mut node.args)
                    .into_iter()
                    .map(|expr| expr.replace(func))
                    .collect::<Result<_, _>>()?;
                Ok(Expr::Eval(node))
            },
            expr => Ok(expr)
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
    fn describe_prec(&self, prec: u32) -> String {
        match self {
            Expr::Imm(item) => item.describe_prec(prec),
            Expr::Eval(node) => node.describe_prec(prec),
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
        }
    }
}

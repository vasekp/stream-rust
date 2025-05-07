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

    /// Creates a special expression `#(n)` or `$(n)`.
    pub fn new_repl(chr: char, ix: Option<usize>) -> Expr {
        Expr::Repl(Subst::new(chr, ix))
    }

    /// Makes the output of this expression an input to a [`Link`].
    pub fn chain(self, next: Link) -> Expr {
        Expr::Eval(Node{
            head: next.head,
            source: Some(Box::new(self)),
            args: next.args
        })
    }

    pub(in crate::base) fn apply(self, source: &Option<Box<Expr>>, args: &Vec<Expr>) -> Result<Expr, StreamError> {
        match self {
            Expr::Eval(node) => Ok(Expr::Eval(node.apply(source, args)?)),
            Expr::Repl(subst) if subst.kind == SubstKind::Input =>
                match subst.index {
                    None => source.as_ref()
                        .ok_or(StreamError::new("no source provided", self))
                        .map(|boxed| (**boxed).clone()),
                    Some(ix) => args.get(ix - 1)
                        .ok_or(StreamError::new("no such input", self))
                        .cloned(),
                },
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

    pub fn replace(&mut self, func: &impl Fn(Subst) -> Result<Expr, BaseError>) -> Result<(), StreamError> {
        match self {
            Expr::Repl(subst) => {
                *self = try_with!(Expr::Repl(*subst), func(*subst)?);
                Ok(())
            },
            Expr::Eval(node) => node.replace(func),
            Expr::Imm(_) => Ok(())
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
pub struct Subst {
    pub kind: SubstKind,
    pub index: Option<usize>
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SubstKind {
    /// Input slot (`#`, `#ix`)
    Input,
    /// Global variable (TODO)
    Global,
    /// History item (`%`, `%ix`)
    History
}

impl Subst {
    fn new(chr: char, ix: Option<usize>) -> Self {
        let kind = match chr {
            '#' => SubstKind::Input,
            '$' => SubstKind::Global,
            '%' => SubstKind::History,
            _ => panic!("unhandled special character '{chr}' in Subst::new()")
        };
        Subst { kind, index: ix }
    }
}

impl ToString for Subst {
    fn to_string(&self) -> String {
        let chr = match self.kind {
            SubstKind::Input => '#',
            SubstKind::Global => '$',
            SubstKind::History => '%',
        };
        match self.index {
            Some(ix) => format!("{chr}{ix}"),
            None => chr.to_string()
        }
    }
}

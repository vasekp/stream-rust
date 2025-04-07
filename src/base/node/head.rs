use crate::base::*;

/// The head of a [`Node`]. This can either be an identifier (`source.ident(args)`), or a body
/// formed by an entire expression (`source.{body}(args)`). In the latter case, the `source` and
/// `args` are accessed via `#` and `#1`, `#2` etc., respectively.
#[derive(Debug, PartialEq, Clone)]
pub enum Head {
    Symbol(String),
    Oper(String),
    Block(Box<Expr>),
    Args(Box<Head>), /// At-sign (source.head@args)
    Lang(LangItem),
    Repl(char, Option<usize>)
}

// Only for private use in Node::describe_helper.
impl Head {
    pub(crate) fn describe(&self) -> String {
        match self {
            Head::Symbol(s) => s.to_owned(),
            Head::Block(b) => format!("{{{}}}", b.describe()),
            Head::Oper(_) => Default::default(),
            Head::Repl(chr, None) => chr.to_string(),
            Head::Repl(chr, Some(num)) => format!("{chr}{num}"),
            Head::Args(head) => format!("{}@", head.describe()),
            Head::Lang(LangItem::BackRef) => "%".to_owned(),
            Head::Lang(_) => Default::default(),
        }
    }

    pub fn args(head: impl Into<Head>) -> Head {
        Head::Args(Box::new(head.into()))
    }
}

impl<T> From<T> for Head where T: Into<String> {
    fn from(symbol: T) -> Head {
        Head::Symbol(symbol.into())
    }
}

impl From<LangItem> for Head {
    fn from(lang: LangItem) -> Head {
        Head::Lang(lang)
    }
}

impl From<Expr> for Head {
    fn from(expr: Expr) -> Head {
        Head::Block(Box::new(expr))
    }
}

impl From<Item> for Head {
    fn from(expr: Item) -> Head {
        Head::Block(Box::new(expr.into()))
    }
}

impl From<Node> for Head {
    fn from(expr: Node) -> Head {
        Head::Block(Box::new(expr.into()))
    }
}

impl PartialEq<str> for Head {
    fn eq(&self, other: &str) -> bool {
        match self {
            Head::Symbol(sym) => sym == other,
            _ => false
        }
    }
}


/// Special types of [`Head`] for language constructs with special syntax.
#[derive(Debug, PartialEq, Clone)]
pub enum LangItem {
    /// List (`[1, 2, 3]` ~ `$list(1, 2, 3)`)
    List,
    /// Parts (`source[1, 2]` ~ `source.$part(1, 2)`)
    Part,
    /// Colon (`source:func` ~ `source.$map(func)`)
    Map,
    /// Back-reference (`%` within `self{...}`)
    BackRef,
}

impl LangItem {
    pub(crate) fn keyword(&self) -> &'static str {
        use LangItem::*;
        match self {
            List => "$list",
            Part => "$part",
            Map => "$map",
            BackRef => "$backref",
        }
    }
}

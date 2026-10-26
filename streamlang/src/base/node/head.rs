use crate::base::*;
use crate::interner::intern;

/// The head of a [`Node`]. This can be an identifier (`source.ident(args)`), an operator
/// (`arg1 + arg2`, `-arg`), a body formed by an entire expression (`source.{body}(args)`), or one
/// of the language constructs with their own syntax ([`LangItem`]). In the case of a body, the
/// `source` and `args` are accessed via `#` and `#1`, `#2` etc. (`##` for the whole argument
/// list), respectively.
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Head {
    /// An identifier, interned.
    Symbol(&'static str),
    /// An operator, interned. The operands are the `args`, the `source` is never set.
    Oper(&'static str),
    /// An expression used in place of an identifier. With `reset_env` set (written `${body}`,
    /// used for functions defined at the session level), the body is evaluated in the default
    /// environment instead of the one in effect at the call site.
    Block { body: Expr, reset_env: bool },
    /// A language construct.
    Lang(LangItem),
}

impl Head {
    pub(crate) fn as_str(&self) -> Option<&'static str> {
        match self {
            Head::Symbol(s) | Head::Oper(s) => Some(s),
            _ => None,
        }
    }
}

impl From<&str> for Head {
    fn from(symbol: &str) -> Head {
        Head::Symbol(intern(symbol))
    }
}

impl From<LangItem> for Head {
    fn from(lang: LangItem) -> Head {
        Head::Lang(lang)
    }
}

impl From<Expr> for Head {
    fn from(expr: Expr) -> Head {
        Head::Block { body: expr, reset_env: false }
    }
}

impl From<Item> for Head {
    fn from(expr: Item) -> Head {
        Head::Block { body: expr.into(), reset_env: false }
    }
}

impl From<Node> for Head {
    fn from(expr: Node) -> Head {
        Head::Block { body: expr.into(), reset_env: false }
    }
}

impl PartialEq<str> for Head {
    fn eq(&self, other: &str) -> bool {
        match self {
            Head::Symbol(sym) | Head::Oper(sym) => *sym == other,
            _ => false,
        }
    }
}

/// Special types of [`Head`] for language constructs with special syntax.
#[derive(Debug, PartialEq, Clone)]
pub enum LangItem {
    /// List (`[1, 2, 3]` ~ `[list](1, 2, 3)`)
    List,
    /// Parts (`source[1, 2]` ~ `source.[part](1, 2)`)
    Part,
    /// Colon (`source:func` ~ `source.[map](func)`)
    Map,
    /// Args (`source.head@args`)
    Args,
}

impl LangItem {
    pub(crate) fn symbol(&self) -> &'static str {
        use LangItem::*;
        match self {
            List => "[list]",
            Part => "[part]",
            Map => "[map]",
            Args => "[args]",
        }
    }
}

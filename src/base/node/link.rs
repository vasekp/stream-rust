use crate::base::*;

/// A precursor of [`Node`] which type-guarantees that the source is left empty.
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Link {
    pub head: Head,
    pub args: Vec<Expr>
}

impl Link {
    /// Creates a new `Link`. The `head` may be specified by [`Head`] directly, but also by
    /// anything implementing `Into<String>` ([`Head::Symbol`]), [`LangItem`] ([`Head::Lang`]),
    /// [`Expr`], [`Item`] or [`Node`] (all three for [`Head::Block`]).
    pub fn new(head: impl Into<Head>, args: Vec<Expr>) -> Link {
        Link{head: head.into(), args}
    }
}

impl From<Link> for Node {
    fn from(prenode: Link) -> Node {
        Node{head: prenode.head, source: None, args: prenode.args}
    }
}

use crate::base::*;

/// A variant of [`Node`] in which all the arguments and source are type-guaranteed to be evaluated.
/// This is achieved by using [`Item`] instead of [`Expr`], avoiding the possibility of [`Expr::Eval`].
#[derive(Debug, PartialEq, Clone)]
pub(crate) struct ENode {
    pub head: Head,
    pub source: Option<Item>,
    pub args: Vec<Item>
}

impl From<ENode> for Node {
    fn from(enode: ENode) -> Node {
        Node {
            head: enode.head,
            source: enode.source.map(|item| Box::new(item.into())),
            args: enode.args.into_iter().map(Expr::from).collect()
        }
    }
}

impl From<&ENode> for Node {
    fn from(enode: &ENode) -> Node {
        Node::from(enode.clone())
    }
}

impl Describe for ENode {
    fn describe(&self) -> String {
        Node::describe_helper(&self.head, self.source.as_ref(), &self.args)
    }
}

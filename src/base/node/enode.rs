use crate::base::*;

/// A variant of [`Node`] in which all the arguments and source are type-guaranteed to be evaluated.
/// This is achieved by using [`Item`] instead of [`Expr`], avoiding the possibility of [`Expr::Eval`].
#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
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
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, self.source.as_ref(), &self.args, prec, env)
    }
}

impl ENode {
    pub(crate) fn resolve(self) -> RNode<Item> {
        match self.source {
            Some(source) => RNode::Source(RNodeS { head: self.head, source, args: self.args.into() }),
            None => RNode::NoSource(RNodeNS { head: self.head, args: self.args.into() }),
        }
    }

    pub(crate) fn resolve_source(self) -> Result<RNodeS<Item>, StreamError> {
        match self.source {
            Some(source) => Ok(RNodeS { head: self.head, source, args: self.args.into() }),
            None => Err(StreamError::new("source required", self))
        }
    }

    pub(crate) fn resolve_no_source(self) -> Result<RNodeNS<Item>, StreamError> {
        match self.source {
            Some(_) => Err(StreamError::new("no source accepted", self)),
            None => Ok(RNodeNS { head: self.head, args: self.args.into() })
        }
    }
}

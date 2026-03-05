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
            source: enode.source.map(Expr::from),
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
        DescribeBuilder::new(&self.head, env)
            .set_source_opt(&self.source)
            .push_args(&self.args)
            .finish(prec)
    }
}

use crate::base::*;

pub(crate) enum RArgs {
    Zero,
    One(Item),
    Two(Item, Item),
    Three(Item, Item, Item),
    More(Vec<Item>)
}

pub(crate) struct RNodeS {
    pub(crate) head: Head,
    pub(crate) source: Item,
    pub(crate) args: RArgs
}

pub(crate) struct RNodeNS {
    pub(crate) head: Head,
    pub(crate) args: RArgs
}

#[allow(unused)]
pub(crate) enum RNode {
    Source(RNodeS),
    NoSource(RNodeNS),
}

impl From<Vec<Item>> for RArgs {
    fn from(mut vec: Vec<Item>) -> RArgs {
        match vec[..] {
            [] => RArgs::Zero,
            [ref mut a1] => RArgs::One(std::mem::take(a1)),
            [ref mut a1, ref mut a2] => RArgs::Two(std::mem::take(a1), std::mem::take(a2)),
            [ref mut a1, ref mut a2, ref mut a3]
                => RArgs::Three(std::mem::take(a1), std::mem::take(a2), std::mem::take(a3)),
            _ => RArgs::More(vec)
        }
    }
}

impl From<RArgs> for Vec<Item> {
    fn from(args: RArgs) -> Self {
        match args {
            RArgs::Zero => vec![],
            RArgs::One(a1) => vec![a1],
            RArgs::Two(a1, a2) => vec![a1, a2],
            RArgs::Three(a1, a2, a3) => vec![a1, a2, a3],
            RArgs::More(vec) => vec
        }
    }
}

impl From<RArgs> for Vec<Expr> {
    fn from(args: RArgs) -> Self {
        match args {
            RArgs::Zero => vec![],
            RArgs::One(a1) => vec![a1.into()],
            RArgs::Two(a1, a2) => vec![a1.into(), a2.into()],
            RArgs::Three(a1, a2, a3) => vec![a1.into(), a2.into(), a3.into()],
            RArgs::More(vec) => vec.into_iter().map(Expr::from).collect()
        }
    }
}

impl From<RNode> for Node {
    fn from(rnode: RNode) -> Node {
        match rnode {
            RNode::Source(r) => r.into(),
            RNode::NoSource(r) => r.into()
        }
    }
}

impl From<RNodeS> for Node {
    fn from(RNodeS { head, source, args }: RNodeS) -> Node {
        Node { head, source: Some(Box::new(source.into())), args: args.into() }
    }
}

impl From<RNodeNS> for Node {
    fn from(RNodeNS { head, args }: RNodeNS) -> Node {
        Node { head, source: None, args: args.into() }
    }
}

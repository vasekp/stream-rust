use crate::base::*;

pub(crate) enum RArgs<T> {
    Zero,
    One(T),
    Two(T, T),
    Three(T, T, T),
    More(Vec<T>)
}

/*impl<T> RArgs<T> {
    fn len(&self) -> usize {
        use RArgs::*;
        match self {
            Zero => 0,
            One(..) => 1,
            Two(..) => 2,
            Three(..) => 3,
            More(vec) => vec.len()
        }
    }
}*/

pub(crate) struct RNodeS<S, T=S> {
    pub(crate) head: Head,
    pub(crate) source: S,
    pub(crate) args: RArgs<T>
}

pub(crate) struct RNodeNS<T> {
    pub(crate) head: Head,
    pub(crate) args: RArgs<T>
}

#[allow(unused)]
pub(crate) enum RNode<S, T=S> {
    Source(RNodeS<S, T>),
    NoSource(RNodeNS<T>),
}

impl<T: std::default::Default> From<Vec<T>> for RArgs<T> {
    fn from(mut vec: Vec<T>) -> RArgs<T> {
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

impl From<RArgs<Item>> for Vec<Item> {
    fn from(args: RArgs<Item>) -> Self {
        match args {
            RArgs::Zero => vec![],
            RArgs::One(a1) => vec![a1],
            RArgs::Two(a1, a2) => vec![a1, a2],
            RArgs::Three(a1, a2, a3) => vec![a1, a2, a3],
            RArgs::More(vec) => vec
        }
    }
}

impl<T> From<RArgs<T>> for Vec<Expr> where T: Into<Expr> {
    fn from(args: RArgs<T>) -> Self {
        match args {
            RArgs::Zero => vec![],
            RArgs::One(a1) => vec![a1.into()],
            RArgs::Two(a1, a2) => vec![a1.into(), a2.into()],
            RArgs::Three(a1, a2, a3) => vec![a1.into(), a2.into(), a3.into()],
            RArgs::More(vec) => vec.into_iter().map(Into::into).collect()
        }
    }
}

impl<S, T> From<RNode<S, T>> for Node where S: Into<Expr>, T: Into<Expr> {
    fn from(rnode: RNode<S, T>) -> Node {
        match rnode {
            RNode::Source(r) => r.into(),
            RNode::NoSource(r) => r.into()
        }
    }
}

impl<S, T> From<RNodeS<S, T>> for Node where S: Into<Expr>, T: Into<Expr> {
    fn from(RNodeS { head, source, args }: RNodeS<S, T>) -> Node {
        Node { head, source: Some(Box::new(source.into())), args: args.into() }
    }
}

impl<T> From<RNodeNS<T>> for Node where T: Into<Expr> {
    fn from(RNodeNS { head, args }: RNodeNS<T>) -> Node {
        Node { head, source: None, args: args.into() }
    }
}

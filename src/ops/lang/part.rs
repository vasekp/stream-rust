use crate::base::*;

fn eval_part(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_source()?);
    try_with!(node, node.check_args_nonempty()?);
    eval_enode(node, env)
}

fn eval_enode(mut node: ENode, env: &Env) -> Result<Item, StreamError> {
    match node.args.remove(0) {
        Item::Number(index) => {
            let snode = ENode{head: LangItem::Part.into(), source: node.source, args: vec![Item::Number(index.clone())]};
            let part = try_with!(snode, match snode.source.as_ref().unwrap() { // source checked before calling eval_enode
                Item::Stream(stm) => eval_index_impl(&**stm, &index),
                Item::String(stm) => eval_index_impl(&**stm, &index).map(Item::Char),
                _ => return Err("expected stream or string".into())
            }?);
            if node.args.is_empty() {
                Ok(part)
            } else {
                let nnode = ENode{head: node.head, source: Some(part), args: node.args};
                eval_enode(nnode, env)
            }
        },
        Item::Stream(stm) => {
            match node.source {
                Some(Item::Stream(source)) =>
                    Ok(Item::new_stream(Part{source: source.into(), indices: stm.into(), rest: node.args, env: env.clone(), head: node.head})),
                Some(Item::String(source)) =>
                    Ok(Item::new_stream(Part{source: source.into(), indices: stm.into(), rest: node.args, env: env.clone(), head: node.head})),
                _ => {
                    node.args.insert(0, Item::Stream(stm));
                    Err(StreamError::new("expected stream or string", node))
                }
            }
        },
        first => {
            Err(StreamError::new(format!("expected number or stream, found {:?}", first), node))
        }
    }
}

fn eval_index_impl<ItemType: ItemTypeT>(source: &dyn Stream<ItemType>, index: &Number) -> Result<ItemType, BaseError> {
    let index = UNumber::try_from(index).map_err(|_| "index must be greater than zero")?;
    if index.is_zero() {
        return Err("index must be greater than zero".into());
    }
    match source.length() {
        Length::Exact(len) | Length::AtMost(len) if len < index =>
            return Err("index past end of stream".into()),
        _ => ()
    }
    let mut iter = source.iter();
    if iter.skip_n(index - 1u32)?.is_some() {
        drop(iter);
        return Err("index past end of stream".into());
    }
    match iter.next() {
        Some(value) => Ok(value?),
        None => {
            drop(iter);
            Err("index past end of stream".into())
        }
    }
}

#[derive(Clone)]
struct Part<ItemType: ItemTypeT> {
    source: BoxedStream<ItemType>,
    indices: BoxedStream,
    rest: Vec<Item>,
    env: Env,
    head: Head
}

impl<ItemType: ItemTypeT> Stream for Part<ItemType> {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(PartIter{parent: self, iter: self.indices.iter()})
    }
}

impl<ItemType: ItemTypeT> Describe for Part<ItemType> {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source),
            std::iter::once(ProxyItem::Stream(&*self.indices))
                .chain(self.rest.iter().map(ProxyItem::from)),
            prec, env)
    }
}

struct PartIter<'node, ItemType: ItemTypeT> {
    parent: &'node Part<ItemType>,
    iter: Box<dyn SIterator + 'node>
}

impl<ItemType: ItemTypeT> Iterator for PartIter<'_, ItemType> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let part = self.iter.next()?;
        let Ok(part) = part else {
            return Some(part);
        };
        // TODO: smarter - number tracks increments, stream unfolds?
        let mut args = self.parent.rest.clone();
        args.insert(0, part);
        let node = ENode {
            head: LangItem::Part.into(),
            source: Some(ItemType::from_box(self.parent.source.clone().into())),
            args
        };
        Some(eval_enode(node, &self.parent.env))
    }
}

impl<ItemType: ItemTypeT> SIterator for PartIter<'_, ItemType> {
    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.iter.skip_n(n)
    }

    fn len_remain(&self) -> Length {
        self.iter.len_remain()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part() {
        use crate::parser::parse;
        test_eval!("range(3)[1]" => "1");
        test_eval!("range(3)[3]" => "3");
        test_eval!("range(3)[4]" => err);
        test_eval!("range(3)[10]" => err);
        test_eval!("range(3)[0]" => err);
        test_eval!("range(3)[-1]" => err);
        test_eval!("[[1,2],[3,4]][2,1]" => "3");
        test_eval!("[[1,2],[3,4]][2][1]" => "3");
        test_eval!("[[1,2],[3,4]].part(2,1)" => "3");
        test_eval!("\"abc\"[2]" => "'b'");
        test_eval!("\"abc\"[[2,3]]" => "['b', 'c']");

        test_eval!("seq(5,2)[100.repeat]" => "[203, 203, 203, 203, 203, ...]");
        test_eval!("seq(5,2)[2*seq+1]" => "[9, 13, 17, 21, 25, ...]");
        test_eval!("seq[seq][seq]" => "[1, 2, 3, 4, 5, ...]");
        test_eval!("seq[seq, seq]" => "[<!>");
        test_eval!("seq:{seq^#}[seq,4]" => "[4, 16, 64, 256, 1024, ...]");
        test_eval!("seq:{seq^#}[seq][4]" => "[1, 16, 81, 256, 625, ...]");
        test_eval!("seq:{seq^#}[4,seq]" => "[1, 16, 81, 256, 625, ...]");
        test_eval!("seq:{seq^#}[4][seq]" => "[1, 16, 81, 256, 625, ...]");
        test_eval!("seq:{seq^#}[[1,2],[1,2,3]]" => "[[1, 2, 3], [...]]");
        test_eval!("seq[2,5]" => err);
        test_eval!("seq[[2,5]]" => "[2, 5]");
        test_eval!("seq[[[2,5]]]" => "[[2, 5]]"); // subject to change
        test_len!("seq[[2,5]]" => 2);
        test_len!("seq[[]]" => 0);

        test_describe!("seq[[3]]" => "seq[[3]]");
        test_describe!("seq.part([3])" => "seq.part([3])");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("*part", eval_part);
    keywords.insert("part", eval_part);
}

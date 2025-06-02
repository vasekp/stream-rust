use crate::base::*;
use crate::utils::NumWithin;

#[derive(Clone)]
struct Part {
    source: BoxedStream,
    indices: BoxedStream,
    rest: Vec<Item>,
    env: Env,
    head: Head
}

impl Part {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        Self::eval_enode(node.eval_all(env)?, env)
    }

    fn eval_enode(mut node: ENode, env: &Env) -> Result<Item, StreamError> {
        let source = match &node.source {
            Some(Item::Stream(stm)) | Some(Item::String(stm)) => stm,
            Some(item) => return Err(StreamError::new(format!("expected stream or string, found {:?}", item), node)),
            _ => return Err(StreamError::new("source required", node))
        };
        match node.args.first() {
            None => Err(StreamError::new("at least 1 argument required", node)),
            Some(Item::Number(index)) => {
                try_with!(node, index.check_within(Number::one()..)?);
                let index = UNumber::try_from(index).unwrap(); // checked above
                match source.length() {
                    Length::Exact(len) | Length::AtMost(len) if len < index =>
                        return Err(StreamError::new("index past end of stream", node)),
                    _ => ()
                }
                let mut iter = source.iter();
                if iter.skip_n(index - 1u32)?.is_some() {
                    drop(iter);
                    return Err(StreamError::new("index past end of stream", node));
                }
                let item = match iter.next() {
                    Some(value) => value?,
                    None => {
                        drop(iter);
                        return Err(StreamError::new("index past end of stream", node));
                    }
                };
                node.args.remove(0);
                if node.args.is_empty() {
                    Ok(item)
                } else {
                    Part::eval_enode(ENode { head: node.head, source: item.into(), args: node.args }, env)
                }
            },
            Some(Item::Stream(_)) => {
                let source = match node.source {
                    Some(Item::Stream(stm)) | Some(Item::String(stm)) => stm,
                    _ => unreachable!()
                };
                let indices = match node.args.remove(0) {
                    Item::Stream(stm) => stm,
                    _ => unreachable!()
                };
                Ok(Item::new_stream(Part{source: source.into(), indices: indices.into(), rest: node.args, env: env.clone(), head: node.head}))
            },
            Some(first) => {
                Err(StreamError::new(format!("expected number or stream, found {:?}", first), node))
            }
        }
    }
}

impl Stream for Part {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(PartIter{parent: self, iter: self.indices.iter()})
    }
}

impl Describe for Part {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source),
            std::iter::once(ProxyItem::Stream(&*self.indices))
                .chain(self.rest.iter().map(ProxyItem::from)),
            prec, env)
    }
}

struct PartIter<'node> {
    parent: &'node Part,
    iter: Box<dyn SIterator + 'node>
}

impl Iterator for PartIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let part = self.iter.next()?;
        let Ok(part) = part else {
            return Some(part);
        };
        // TODO: smarter - number tracks increments, stream unfolds?
        let mut args = self.parent.rest.clone();
        args.insert(0, part);
        let node = ENode { head: LangItem::Part.into(), source: Some(Item::Stream(self.parent.source.clone().into())), args };
        Some(Part::eval_enode(node, &self.parent.env))
    }
}

impl SIterator for PartIter<'_> {
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
    keywords.insert("*part", Part::eval);
    keywords.insert("part", Part::eval);
}

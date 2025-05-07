use crate::base::*;
use crate::utils::NumWithin;

#[derive(Clone)]
struct Part {
    source: BoxedStream,
    indices: BoxedStream,
    rest: Vec<Item>,
    env: Rc<Env>
}

impl Part {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        Self::eval_enode(node.eval_all(env)?, env)
    }

    fn eval_enode(mut node: ENode, env: &Rc<Env>) -> Result<Item, StreamError> {
        let source = try_with!(node, node.source_checked()?.to_stream()?);
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
                    return Err(StreamError::new("index past end of stream", node));
                }
                let item = match iter.next() {
                    Some(value) => value?,
                    None => return Err(StreamError::new("index past end of stream", node))
                };
                node.args.remove(0);
                if node.args.is_empty() {
                    Ok(item)
                } else {
                    Part::eval_enode(ENode { head: node.head, source: item.into(), args: node.args }, env)
                }
            },
            Some(Item::Stream(_)) => {
                let indices = node.args.remove(0).to_stream().unwrap();
                Ok(Item::new_stream(Part{source: source.into(), indices: indices.into(), rest: node.args, env: Rc::clone(env)}))
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
    fn describe_prec(&self, prec: u32) -> String {
        Node::describe_helper(&Head::Lang(LangItem::Part), Some(&self.source),
            [self.indices.to_item()].iter().chain(self.rest.iter()), prec)
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
        let node = ENode { head: LangItem::Part.into(), source: Some(self.parent.source.to_item()), args };
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
        assert_eq!(parse("range(3)[1]").unwrap().eval_default().unwrap().to_string(), "1");
        assert_eq!(parse("range(3)[3]").unwrap().eval_default().unwrap().to_string(), "3");
        assert!(parse("range(3)[4]").unwrap().eval_default().is_err());
        assert!(parse("range(3)[10]").unwrap().eval_default().is_err());
        assert!(parse("range(3)[0]").unwrap().eval_default().is_err());
        assert!(parse("range(3)[-1]").unwrap().eval_default().is_err());
        assert_eq!(parse("[[1,2],[3,4]][2,1]").unwrap().eval_default().unwrap().to_string(), "3");
        assert_eq!(parse("[[1,2],[3,4]][2][1]").unwrap().eval_default().unwrap().to_string(), "3");

        assert_eq!(parse("seq(5,2)[100.repeat]").unwrap().eval_default().unwrap().to_string(), "[203, 203, 203, 203, 203, ...]");
        assert_eq!(parse("seq(5,2)[2*seq+1]").unwrap().eval_default().unwrap().to_string(), "[9, 13, 17, 21, 25, ...]");
        assert_eq!(parse("seq[seq][seq]").unwrap().eval_default().unwrap().to_string(), "[1, 2, 3, 4, 5, ...]");
        assert_eq!(parse("seq[seq, seq]").unwrap().eval_default().unwrap().to_string(), "[<!>");
        assert_eq!(parse("seq:{seq^#}[seq,4]").unwrap().eval_default().unwrap().to_string(), "[4, 16, 64, 256, 1024, ...]");
        assert_eq!(parse("seq:{seq^#}[seq][4]").unwrap().eval_default().unwrap().to_string(), "[1, 16, 81, 256, 625, ...]");
        assert_eq!(parse("seq:{seq^#}[4,seq]").unwrap().eval_default().unwrap().to_string(), "[1, 16, 81, 256, 625, ...]");
        assert_eq!(parse("seq:{seq^#}[4][seq]").unwrap().eval_default().unwrap().to_string(), "[1, 16, 81, 256, 625, ...]");
        assert_eq!(parse("seq:{seq^#}[[1,2],[1,2,3]]").unwrap().eval_default().unwrap().to_string(), "[[1, 2, 3], [...]]");
        assert!(parse("seq[2,5]").unwrap().eval_default().is_err());
        assert_eq!(parse("seq[[2,5]]").unwrap().eval_default().unwrap().to_string(), "[2, 5]");
        assert_eq!(parse("seq[[[2,5]]]").unwrap().eval_default().unwrap().to_string(), "[[2, 5]]"); // subject to change
        test_len_exact(&parse("seq[[2,5]]").unwrap().eval_default().unwrap(), 2);
        test_len_exact(&parse("seq[[]]").unwrap().eval_default().unwrap(), 0);

        assert_eq!(parse("seq[[3]]").unwrap().eval_default().unwrap().describe(), "seq[[3]]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("*part", Part::eval);
}

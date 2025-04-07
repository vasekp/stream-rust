use crate::base::*;
use crate::utils::NumWithin;

use std::rc::Rc;

#[derive(Clone)]
struct Part {
    source: BoxedStream,
    indices: BoxedStream,
    rest: Vec<Expr>,
    env: Rc<Env>
}

impl Part {
    fn eval(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
        use once_cell::unsync::Lazy;
        let mut node = node.eval_source(env)?;
        let source = try_with!(node, node.source_checked()?.as_item()?.to_stream()?);
        let length = Lazy::new(|| match source.length() {
            Length::Exact(len) => Ok(len),
            Length::Infinite => Err(BaseError::from("stream is infinite")),
            _ => Ok(source.iter().count().into())
        });
        type R = Result<UNumber, BaseError>;
        fn subs_len(expr: &mut Expr, length: &Lazy<R, impl Fn() -> R>) ->
            Result<(), BaseError>
        {
            match expr {
                Expr::Imm(_) => Ok(()),
                Expr::Eval(node) => {
                    if &node.head == "len" && node.source.is_none() {
                        match Lazy::force(length) {
                            Ok(len) => *expr = Expr::new_number(len.to_owned()),
                            Err(err) => return Err(err.clone())
                        }
                        return Ok(());
                    }
                    node.source.iter_mut().try_for_each(|sbox| subs_len(sbox, length))?;
                    match &mut node.head {
                        Head::Lang(LangItem::Part) => return Ok(()), // $part does not enter into args
                        Head::Block(expr) => subs_len(expr, length)?,
                        Head::Args(head) => {
                            if let Head::Block(ref mut expr) = **head {
                                subs_len(expr, length)?
                            }
                        },
                        _ => ()
                    }
                    node.args.iter_mut().try_for_each(|arg| subs_len(arg, length))?;
                    Ok(())
                }
            }
        }
        try_with!(node, subs_len(node.first_arg_checked_mut()?, &length)?);
        let first = node.args.remove(0).eval_env(env)?;
        match first {
            Item::Number(index) => {
                macro_rules! orig_node {
                    () => { {
                        node.args.insert(0, Expr::new_number(index));
                        node
                    } }
                }
                try_with!(orig_node!(), index.check_within(Number::one()..)?);
                let index = UNumber::try_from(&index).unwrap(); // checked above
                match source.length() {
                    Length::Exact(len) | Length::AtMost(len) if len < index =>
                        return Err(StreamError::new("index past end of stream", orig_node!())),
                    _ => ()
                }
                let mut iter = source.iter();
                if iter.skip_n(index - 1u32)?.is_some() {
                    return Err(StreamError::new("index past end of stream", orig_node!()));
                }
                let item = match iter.next() {
                    Some(value) => value?,
                    None => return Err(StreamError::new("index past end of stream", orig_node!()))
                };
                if node.args.is_empty() {
                    Ok(item)
                } else {
                    Part::eval(Node::new(node.head, Some(item.into()), node.args), env)
                }
            },
            Item::Stream(indices) => {
                Ok(Item::new_stream(Part{source: source.into(), indices: indices.into(), rest: node.args, env: Rc::clone(env)}))
            },
            item => {
                node.args.insert(0, item.into());
                Err(StreamError::new(format!("expected number or stream, found {:?}", node.args[0]), node))
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
    fn describe(&self) -> String {
        Node::describe_helper(&Head::Lang(LangItem::Part), Some(&self.source), 
            [&self.indices.to_expr()].into_iter().chain(self.rest.iter()))
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
        args.insert(0, Expr::Imm(part));
        let node = Node::new(LangItem::Part, Some(self.parent.source.to_expr()), args);
        Some(Part::eval(node, &self.parent.env))
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
        assert_eq!(parse("range(3)[1]").unwrap().eval().unwrap().to_string(), "1");
        assert_eq!(parse("range(3)[3]").unwrap().eval().unwrap().to_string(), "3");
        assert!(parse("range(3)[4]").unwrap().eval().is_err());
        assert!(parse("range(3)[10]").unwrap().eval().is_err());
        assert!(parse("range(3)[0]").unwrap().eval().is_err());
        assert!(parse("range(3)[-1]").unwrap().eval().is_err());
        assert_eq!(parse("[[1,2],[3,4]][2,1]").unwrap().eval().unwrap().to_string(), "3");
        assert_eq!(parse("[[1,2],[3,4]][2][1]").unwrap().eval().unwrap().to_string(), "3");

        assert_eq!(parse("seq(5,2)[100.repeat]").unwrap().eval().unwrap().to_string(), "[203, 203, 203, 203, 203, ...]");
        assert_eq!(parse("seq(5,2)[2*seq+1]").unwrap().eval().unwrap().to_string(), "[9, 13, 17, 21, 25, ...]");
        assert_eq!(parse("seq[seq][seq]").unwrap().eval().unwrap().to_string(), "[1, 2, 3, 4, 5, ...]");
        assert_eq!(parse("seq[seq, seq]").unwrap().eval().unwrap().to_string(), "[<!>");
        assert_eq!(parse("seq:{seq^#}[seq,4]").unwrap().eval().unwrap().to_string(), "[4, 16, 64, 256, 1024, ...]");
        assert_eq!(parse("seq:{seq^#}[seq][4]").unwrap().eval().unwrap().to_string(), "[1, 16, 81, 256, 625, ...]");
        assert_eq!(parse("seq:{seq^#}[4,seq]").unwrap().eval().unwrap().to_string(), "[1, 16, 81, 256, 625, ...]");
        assert_eq!(parse("seq:{seq^#}[4][seq]").unwrap().eval().unwrap().to_string(), "[1, 16, 81, 256, 625, ...]");
        assert_eq!(parse("seq:{seq^#}[[1,2],[1,2,3]]").unwrap().eval().unwrap().to_string(), "[[1, 2, 3], [...]]");
        assert!(parse("seq[2,5]").unwrap().eval().is_err());
        assert_eq!(parse("seq[[2,5]]").unwrap().eval().unwrap().to_string(), "[2, 5]");
        assert_eq!(parse("seq[[[2,5]]]").unwrap().eval().unwrap().to_string(), "[[2, 5]]"); // subject to change
        test_len_exact(&parse("seq[[2,5]]").unwrap().eval().unwrap(), 2);
        test_len_exact(&parse("seq[[]]").unwrap().eval().unwrap(), 0);

        assert_eq!(parse("[1,2,3][len]").unwrap().eval().unwrap().to_string(), "3");
        assert_eq!(parse("[1,2,3][[len]]").unwrap().eval().unwrap().to_string(), "[3]");
        assert_eq!(parse("[1,2,3][len-seq]").unwrap().eval().unwrap().to_string(), "[2, 1, <!>");
        assert_eq!(parse("[[1], [1,2,3]][len, len]").unwrap().eval().unwrap().to_string(), "3");
        assert_eq!(parse("[[1], [1,2,3]][1..2, len]").unwrap().eval().unwrap().to_string(), "[1, 3]");
        assert_eq!(parse("[1,2,3][[1,2].len]").unwrap().eval().unwrap().to_string(), "2");
        assert!(parse("[1,2,3][seq[len]]").unwrap().eval().is_err());
        assert_eq!(parse("[1,2,3][range(len-1)[len]]").unwrap().eval().unwrap().to_string(), "2");
        assert_eq!(parse("[1,2,3,4][{#1..(#1+1)}(len/2)]").unwrap().eval().unwrap().to_string(), "[2, 3]");
        assert_eq!(parse("[1,2,3][{len}]").unwrap().eval().unwrap().to_string(), "3");
        assert_eq!(parse("[1,2,3][{len}@[]]").unwrap().eval().unwrap().to_string(), "3");
        assert_eq!(parse("seq[3]").unwrap().eval().unwrap().describe(), "3");
        assert_eq!(parse("range(5)[len]").unwrap().eval().unwrap().describe(), "5");

        assert_eq!(parse("seq[[3]]").unwrap().eval().unwrap().describe(), "seq[[3]]");
        assert_eq!(parse("range(5)[[1],len]").unwrap().eval().unwrap().describe(), "range(5)[[1], len]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("$part", Part::eval);
}

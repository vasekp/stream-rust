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
                Some(Item::String(source)) if node.args.is_empty() =>
                    Ok(Item::new_string(StringPart{source: source.into(), indices: stm.into(), head: node.head})),
                Some(Item::String(_)) => {
                    node.args.insert(0, Item::Stream(stm));
                    Err(StreamError::new("expected only one level of parts", node))
                },
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

fn eval_index_impl<I: ItemType>(source: &dyn Stream<I>, index: &Number) -> Result<I, BaseError> {
    let index = UNumber::try_from(index).map_err(|_| "index must be greater than zero")?;
    if index.is_zero() {
        return Err("index must be greater than zero".into());
    }
    match source.len() {
        Length::Exact(len) | Length::AtMost(len) if len < index =>
            return Err("index past end of stream".into()),
        _ => ()
    }
    let mut iter = source.iter();
    if iter.advance(index - 1u32)?.is_some() {
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
struct Part {
    source: BoxedStream,
    indices: BoxedStream,
    rest: Vec<Item>,
    env: Env,
    head: Head
}

impl Stream for Part {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(PartIter{parent: self, iter: self.indices.iter()})
    }
}

impl Describe for Part {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new_with_env(&self.head, env, &self.env)
            .set_source(&self.source)
            .push_arg(&*self.indices)
            .push_args(&self.rest)
            .finish(prec)
    }
}

struct PartIter<'node> {
    parent: &'node Part,
    iter: Box<dyn SIterator + 'node>
}

impl Iterator for PartIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let part = iter_try_expr!(self.iter.next()?);
        // TODO: smarter - number tracks increments, stream unfolds?
        let mut args = self.parent.rest.clone();
        args.insert(0, part);
        let node = ENode {
            head: LangItem::Part.into(),
            source: Some(self.parent.source.clone().into()),
            args
        };
        Some(eval_enode(node, &self.parent.env))
    }
}

impl SIterator for PartIter<'_> {
    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.iter.advance(n)
    }

    fn len_remain(&self) -> Length {
        self.iter.len_remain()
    }
}

#[derive(Clone)]
struct StringPart {
    source: BoxedStream<Char>,
    indices: BoxedStream,
    head: Head
}

impl Stream<Char> for StringPart {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<Char> + 'node> {
        Box::new(StringPartIter{parent: self, iter: self.indices.iter()})
    }
}

impl Describe for StringPart {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_arg(&*self.indices)
            .finish(prec)
    }
}

struct StringPartIter<'node> {
    parent: &'node StringPart,
    iter: Box<dyn SIterator + 'node>
}

impl Iterator for StringPartIter<'_> {
    type Item = Result<Char, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        match iter_try_expr!(self.iter.next()?) {
            Item::Number(index) => Some(eval_index_impl(&*self.parent.source, &index)
                .map_err(|err| StreamError::new(err, Item::from(self.parent.source.clone())))),
            _ => todo!()
        }
    }
}

impl SIterator<Char> for StringPartIter<'_> {
    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.iter.advance(n)
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
        test_eval!("\"abcdef\"[3..5]" => "\"cde\"");

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

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("*part", eval_part);
    symbols.insert_with_docs("part", eval_part, r#"
In its simplest form `stream.?(index)`, evaluates to the `index`-th item in `stream`.
Part specifications can be given to greater depth using multiple arguments.
If a certain argument is a stream of indices, evaluates to a stream of concrete part specifications.
The shorthand for `stream.?(arg1, arg2, ...)` is `stream[arg1, arg2, ...]`.
= stream.?(arg1, arg2, ...)
= string.?(index or stream)
= stream[arg1, arg2, ...]
= string[index or stream]
> ?range(10, 20).?(2) => 11
> ?range(10, 20).?([2, 5]) => [11, 14]
> ?range(10, 20).?(2..5) => [11, 12, 13, 14]
> [[1, 2], ['a', 'b']].?(2, 1) => 'a' ; take second item (['a', 'b']), then first item of that
> "abcde".?(3..4) => "cd"
: range
"#);
}

use crate::base::*;

fn eval_part(node: &Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    node.check_source()?;
    node.check_args_nonempty()?;
    eval_enode(&node, env)
}

fn eval_enode(node: &Node<Item>, env: &Env) -> Result<Item, StreamError> {
    let source = node.source.as_ref().expect("source should be nonempty by precondition");
    match node.args.split_first().expect("args should be nonempty by precondition") {
        (Item::Number(index), rest) => {
            let part = match source {
                Item::Stream(stm) => eval_index_impl(stm, index)?,
                Item::String(stm) => eval_index_impl(stm, index).map(Item::Char)?,
                _ => return Err("expected stream or string".into())
            };
            if rest.is_empty() {
                Ok(part)
            } else {
                let nnode = Node{head: node.head.clone(), source: Some(part), args: rest.to_owned()};
                eval_enode(&nnode, env).map_err(|err| err.wrap(&nnode))
            }
        },
        (Item::Stream(indices), rest) => {
            match source {
                Item::Stream(stm) =>
                    Ok(Item::new_stream(Part{source: stm.clone(), indices: Rc::clone(indices), rest: rest.to_owned(), env: env.clone(), head: node.head.clone()})),
                Item::String(stm) if rest.is_empty() =>
                    Ok(Item::new_string(StringPart{source: stm.clone(), indices: Rc::clone(indices), head: node.head.clone()})),
                Item::String(_) => Err("expected only one level of parts".into()),
                _ => Err("expected stream or string".into())
            }
        },
        _ => Err("expected number or stream".into())
    }
}

fn eval_index_impl<I: ItemType>(source: &Rc<dyn Stream<I>>, index: &Number) -> Result<I, StreamError> {
    let index = index.try_cast_within(UNumber::one()..)?;
    match source.len() {
        Length::Exact(len) | Length::AtMost(len) if len < index =>
            return Err("index past end of stream".into()),
        _ => ()
    }
    let mut iter = source.iter();
    if iter.advance(index - 1u32)?.is_some() {
        return Err("index past end of stream".into());
    }
    match iter.next()? {
        Some(value) => Ok(value),
        None => Err("index past end of stream".into())
    }
}

struct Part {
    source: Rc<dyn Stream>,
    indices: Rc<dyn Stream>,
    rest: Vec<Item>,
    env: Env,
    head: Head
}

impl Stream for Part {
    fn iter(&self) -> Result<Box<dyn SIterator + '_>, StreamError> {
        Ok(Box::new(PartIter{parent: self, iter: self.indices.iter()}))
    }

    fn len(&self) -> Length {
        self.indices.len()
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

impl SIterator for PartIter<'_> {
    fn next(&mut self) -> Result<Option<Item>, StreamError> {
        let part = iter_try!(self.iter.next());
        // TODO: smarter - number tracks increments, stream unfolds?
        let mut args = self.parent.rest.clone();
        args.insert(0, part);
        let node = Node {
            head: LangItem::Part.into(),
            source: Some(self.parent.source.clone().into()),
            args
        };
        eval_enode(&node, &self.parent.env)
            .map(Option::Some)
            .map_err(|err| err.wrap(&node))
    }

    fn advance(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.iter.advance(n)
    }

    fn len_remain(&self) -> Length {
        self.iter.len_remain()
    }
}

struct StringPart {
    source: Rc<dyn Stream<Char>>,
    indices: Rc<dyn Stream>,
    head: Head
}

impl Stream<Char> for StringPart {
    fn iter(&self) -> Result<Box<dyn SIterator<Char> + '_>, StreamError> {
        Ok(Box::new(StringPartIter{parent: self, iter: self.indices.iter()}))
    }

    fn len(&self) -> Length {
        self.indices.len()
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

impl SIterator<Char> for StringPartIter<'_> {
    fn next(&mut self) -> Result<Option<Char>, StreamError> {
        match iter_try!(self.iter.next()) {
            Item::Number(index) => eval_index_impl(&self.parent.source, &index).map(Option::Some),
            _ => todo!()
        }
    }

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
    symbols.insert_raw("[part]", eval_part);
    symbols.insert("part", eval_part, r#"
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

use crate::base::*;

#[derive(Clone)]
struct Split {
    head: Head,
    source: BoxedStream,
    sep: Vec<LiteralString>,
}

struct SplitIter<'node> {
    source: StringIterator<'node>,
    sep: &'node Vec<LiteralString>,
    done: bool,
}

fn eval_split(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_args_nonempty()?);
    match &node.source {
        Some(Item::String(_)) => {
            let sep = try_with!(node, node.args.iter()
                .map(|item| match item {
                    Item::Char(ch) => Ok(vec![ch.to_owned()]),
                    Item::String(s) if !s.is_empty() => s.string_listout().map_err(BaseError::from),
                    _ => Err(BaseError::from(format!("expected character or nonempty string, found {:?}", item)))
                })
                .map(|res| res.map(LiteralString::from))
                .collect::<Result<Vec<_>, _>>()?);
            let Some(Item::String(stm)) = node.source else { unreachable!() };
            Ok(Item::new_stream(Split{head: node.head, source: stm.into(), sep}))
        },
        _ => Err(StreamError::new("expected: string.split(separators)", node))
    }
}

impl Describe for Split {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source), &self.sep, prec, env)
    }
}

impl Stream for Split {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(SplitIter{source: self.source.string_iter(), sep: &self.sep, done: false})
    }

    fn length(&self) -> Length {
        Length::at_most(self.source.length())
    }
}

impl Iterator for SplitIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        let mut cache = vec![];
        for item in &mut self.source {
            check_stop!(iter);
            cache.push(iter_try_expr!(item));
            for sep in self.sep {
                if sep.len() > cache.len() { continue; }
                let bkpt = cache.len() - sep.len();
                if cache[bkpt..] == sep[..] {
                    cache.truncate(bkpt);
                    return Some(Ok(Item::new_string_stream(LiteralString::from(cache))));
                }
            }
        }
        self.done = true;
        Some(Ok(Item::new_string_stream(LiteralString::from(cache))))
    }
}

impl SIterator for SplitIter<'_> {
    fn len_remain(&self) -> Length {
        Length::at_most(self.source.len_remain())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_split() {
        use super::*;
        use crate::parser::parse;
        test_eval!("\"Hello, world!\".split(\", \")" => "[\"Hello\", \"world!\"]");
        test_eval!("\"Hello, world!\".split(',', ' ')" => "[\"Hello\", \"\", \"world!\"]");
        test_eval!("\"abbacca\".split('b', \"bb\", \"cc\")" => "[\"a\", \"\", \"a\", \"a\"]");
        test_eval!("\"\".split(' ')" => "[\"\"]");
        test_eval!("\"abc\".split(\"\")" => err);
        test_eval!("\"abc\".split()" => err);
        test_eval!("\"abcacbadc\".split(\"ab\", \"ac\")" => "[\"\", \"c\", \"badc\"]");
        test_describe!("\"Hello, world!\".split(',', ' ')" => "\"Hello, world!\".split(\",\", \" \")");
        test_eval!("\"abcacbadc\".split('a'.repeat)" => err);
        test_eval!("\"abcacbadc\".split('a'.repeat(10^20))" => err);
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("split", eval_split);
}

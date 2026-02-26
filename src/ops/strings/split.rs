use crate::base::*;

fn eval_split(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_args_nonempty()?);
    match node.source {
        Some(Item::String(_)) => {
            let sep = try_with!(node, node.args.iter()
                .map(|item| match item {
                    Item::Char(ch) => Ok(vec![ch.to_owned()]),
                    Item::String(s) if !s.is_empty() => s.listout().map_err(BaseError::from),
                    _ => Err(BaseError::from(format!("expected character or nonempty string, found {:?}", item)))
                })
                .map(|res| res.map(LiteralString::from))
                .collect::<Result<Vec<_>, _>>()?);
            let Some(Item::String(stm)) = node.source else { unreachable!() };
            Ok(Item::new_stream(SplitString{head: node.head, source: stm.into(), sep}))
        },
        Some(Item::Stream(stm)) => {
            Ok(Item::new_stream(SplitStream{head: node.head, source: stm.into(), sep: node.args}))
        },
        _ => Err(StreamError::new("expected: string.split(separators) or stream.split(separators)", node))
    }
}

#[derive(Clone)]
struct SplitString {
    head: Head,
    source: BoxedStream<Char>,
    sep: Vec<LiteralString>,
}

struct SplitStringIter<'node> {
    source: Box<dyn SIterator<Char> + 'node>,
    sep: &'node Vec<LiteralString>,
    done: bool,
}

impl Describe for SplitString {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_args(&self.sep)
            .finish(prec)
    }
}

impl Stream for SplitString {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(SplitStringIter{source: self.source.iter(), sep: &self.sep, done: false})
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

impl Iterator for SplitStringIter<'_> {
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
                if (**sep).len() > cache.len() { continue; }
                let bkpt = cache.len() - (**sep).len();
                if cache[bkpt..] == sep[..] {
                    cache.truncate(bkpt);
                    return Some(Ok(Item::new_string(LiteralString::from(cache))));
                }
            }
        }
        self.done = true;
        Some(Ok(Item::new_string(LiteralString::from(cache))))
    }
}

impl SIterator for SplitStringIter<'_> {
    fn len_remain(&self) -> Length {
        Length::at_most(self.source.len_remain())
    }
}

#[derive(Clone)]
struct SplitStream {
    head: Head,
    source: BoxedStream,
    sep: Vec<Item>,
}

struct SplitStreamIter<'node> {
    source: Box<dyn SIterator + 'node>,
    sep: &'node Vec<Item>,
    done: bool,
}

impl Describe for SplitStream {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .push_args(&self.sep)
            .finish(prec)
    }
}

impl Stream for SplitStream {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(SplitStreamIter{source: self.source.iter(), sep: &self.sep, done: false})
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

impl Iterator for SplitStreamIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        let mut cache = vec![];
        for item in &mut self.source {
            check_stop!(iter);
            let item = iter_try_expr!(item);
            for sep in self.sep {
                if iter_try_expr!(item.try_eq(sep)) {
                    return Some(Ok(Item::new_stream(List::from(cache))));
                }
            }
            cache.push(item);
        }
        self.done = true;
        Some(Ok(Item::new_stream(List::from(cache))))
    }
}

impl SIterator for SplitStreamIter<'_> {
    fn len_remain(&self) -> Length {
        Length::at_most(self.source.len_remain())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_split() {
        use super::*;
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
        test_eval!("range(10).split(3)" => "[[1, 2], [4, ...]]");
        test_eval!("range('a', 'e').split('c')" => "[['a', 'b'], ['d', ...]]");
        test_eval!("range('a', 'e').split(\"c\")" => "[['a', 'b', 'c', 'd', ...]]");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("split", eval_split, r#"
Splits `string` on occurrences of any of the `delimiter`s, which can each be a character or a substring.
For streams: the `delimiter`s are any items, which are compared for exact match.
= string.split(delimiter...)
= stream.split(delimiter...)
> "Hello, world".?(", ") => ["Hello", "world"]
> "Hello, world".?(' ', ',') => ["Hello", "", "world"] ; there's an empty string between ',' and ' '
> "two  spaces".?(" ", "  ") => ["two", "", "spaces"] ; " " is encountered before "  "
> ?range(10).?(3) : 6 => [[1, 2], [4, 5, ...]]
> ?range(10).?([3, 4]) : 6 => [[1, 2, 3, 4, 5, ...]] ; the *item* [3, 4] never appears
: splitby
"#);
}

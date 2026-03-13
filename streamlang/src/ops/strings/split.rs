use crate::base::*;

fn eval_split(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_args_nonempty()?;
    match node.source {
        Some(Item::String(_)) => {
            let sep = node.args.iter()
                .map(|item| match item {
                    Item::Char(ch) => Ok(vec![ch.to_owned()]),
                    Item::String(s) => s.listout_check_nonempty(),
                    _ => Err(StreamError::with_expr("expected character or nonempty string", item))
                })
                .map(|res| res.map(LiteralString::from))
                .collect::<SResult<Vec<_>>>()?;
            let Some(Item::String(stm)) = node.source else { unreachable!() };
            Ok(Item::new_stream(SplitString{head: node.head, source: stm, sep}))
        },
        Some(Item::Stream(stm)) => {
            Ok(Item::new_stream(SplitStream{head: node.head, source: stm, sep: node.args}))
        },
        _ => Err(StreamError::usage(&node.head))
    }
}

struct SplitString {
    head: Head,
    source: Rc<dyn Stream<Char>>,
    sep: Vec<LiteralString>,
}

struct SplitStringIter {
    node: Rc<SplitString>,
    source: Box<dyn SIterator<Char>>,
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
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        SplitStringIter{source: self.source.iter(), done: false, node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

impl PreIterator for SplitStringIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        if self.done {
            return Ok(None);
        }
        let mut cache = vec![];
        for item in self.source.transposed() {
            check_stop!();
            cache.push(item?);
            for sep in &self.node.sep {
                let sep = sep.as_slice();
                if sep.len() > cache.len() { continue; }
                let bkpt = cache.len() - sep.len();
                if cache[bkpt..] == sep[..] {
                    cache.truncate(bkpt);
                    return Ok(Some(Item::new_string(LiteralString::from(cache))));
                }
            }
        }
        self.done = true;
        Ok(Some(Item::new_string(LiteralString::from(cache))))
    }

    fn len_remain(&self) -> Length {
        Length::at_most(self.source.len_remain())
    }

    fn origin(&self) -> &Rc<SplitString> {
        &self.node
    }
}

struct SplitStream {
    head: Head,
    source: Rc<dyn Stream>,
    sep: Vec<Item>,
}

struct SplitStreamIter {
    node: Rc<SplitStream>,
    source: Box<dyn SIterator>,
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
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        SplitStreamIter{source: self.source.iter(), done: false, node: self}.wrap()
    }

    fn len(&self) -> Length {
        Length::at_most(self.source.len())
    }
}

impl PreIterator for SplitStreamIter {
    fn next(&mut self) -> SResult<Option<Item>> {
        if self.done {
            return Ok(None);
        }
        let mut cache = vec![];
        for item in self.source.transposed() {
            check_stop!();
            let item = item?;
            for sep in &self.node.sep {
                if item.try_eq(sep)? {
                    return Ok(Some(Item::new_stream(List::from(cache))));
                }
            }
            cache.push(item);
        }
        self.done = true;
        Ok(Some(Item::new_stream(List::from(cache))))
    }

    fn len_remain(&self) -> Length {
        Length::at_most(self.source.len_remain())
    }

    fn origin(&self) -> &Rc<SplitStream> {
        &self.node
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

use crate::base::*;

#[derive(Clone)]
struct Chars {
    head: Head,
    source: BoxedStream<Char>
}

struct CharsIter<'node> {
    source: Box<dyn SIterator<Char> + 'node>
}

impl Chars {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        try_with!(node, node.check_no_args()?);
        let rnode = node.eval_all(env)?.resolve_source()?;
        match rnode.source {
            Item::String(stm) => Ok(Item::new_stream(Chars{head: rnode.head, source: stm.into()})),
            ref item => Err(StreamError::new(format!("expected string, found {:?}", item), rnode))
        }
    }
}

impl Describe for Chars {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source), None::<&Item>, prec, env)
    }
}

impl Stream for Chars {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        Box::new(CharsIter{source: self.source.iter()})
    }

    fn length(&self) -> Length {
        self.source.length()
    }
}

impl Iterator for CharsIter<'_> {
    type Item = Result<Item, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.source.next()?;
        Some(res.map(Item::Char))
    }
}

impl SIterator for CharsIter<'_> {
    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.source.skip_n(n)
    }

    fn len_remain(&self) -> Length {
        self.source.len_remain()
    }
}


#[derive(Clone)]
struct Str {
    head: Head,
    source: BoxedStream
}

struct StrIter<'node> {
    parent: &'node Str,
    source: Box<dyn SIterator + 'node>
}

impl Str {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        try_with!(node, node.check_no_args()?);
        let rnode = node.eval_all(env)?.resolve_source()?;
        match rnode.source {
            Item::Stream(stm) => Ok(Item::new_string_stream(Str{head: rnode.head, source: stm.into()})),
            ref item => Err(StreamError::new(format!("expected stream, found {:?}", item), rnode))
        }
    }
}

impl Describe for Str {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        Node::describe_helper(&self.head, Some(&self.source), None::<&Item>, prec, env)
    }
}

impl Stream<Char> for Str {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<Char> + 'node> {
        Box::new(StrIter{parent: &self, source: self.source.iter()})
    }

    fn length(&self) -> Length {
        self.source.length()
    }
}

impl Iterator for StrIter<'_> {
    type Item = Result<Char, StreamError>;

    fn next(&mut self) -> Option<Self::Item> {
        let item = iter_try_expr!(self.source.next()?);
        Some(match item.into_char() {
            Ok(ch) => Ok(ch),
            Err(err) => Err(StreamError::new(err, Item::new_string_stream(self.parent.clone())))
        })
    }
}

impl SIterator<Char> for StrIter<'_> {
    fn skip_n(&mut self, n: UNumber) -> Result<Option<UNumber>, StreamError> {
        self.source.skip_n(n)
    }

    fn len_remain(&self) -> Length {
        self.source.len_remain()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_chars_string() {
        use super::*;
        use crate::parser::parse;

        test_eval!("\"abc\".chars" => "['a', 'b', 'c']");
        test_eval!("\"\".chars" => "[]");
        test_eval!("'a'.repeat.chars" => "['a', 'a', 'a', 'a', 'a', ...]");
        test_eval!("\"abc\".chars(1)" => err);
        test_eval!("['a', 'b', 'c'].chars" => err);

        test_eval!("['a', 'b', 'c'].string" => "\"abc\"");
        test_eval!("[].string" => "\"\"");
        test_eval!("'a'.repeat.chars.string" => "\"aaaaaaaaaaaaaaaaaaaa...");
        test_eval!("['a', 'b', \"c\"].string" => "\"ab<!>");
        test_eval!("seq.string" => "\"<!>");
        test_eval!("range('a','c').string" => "\"abc\"");
        test_eval!("\"abc\".string" => err);
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("chars", Chars::eval);
    keywords.insert("string", Str::eval);
}

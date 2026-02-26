use crate::base::*;

#[derive(Clone)]
struct Chars {
    head: Head,
    source: BoxedStream<Char>
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
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .finish(prec)
    }
}

impl Stream for Chars {
    fn iter<'node>(&'node self) -> Box<dyn SIterator + 'node> {
        self.source.map_iter(|ch| Ok(Item::Char(ch)))
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}


#[derive(Clone)]
struct Str {
    head: Head,
    source: BoxedStream
}

impl Str {
    fn eval(node: Node, env: &Env) -> Result<Item, StreamError> {
        try_with!(node, node.check_no_args()?);
        let rnode = node.eval_all(env)?.resolve_source()?;
        match rnode.source {
            Item::Stream(stm) => Ok(Item::new_string(Str{head: rnode.head, source: stm.into()})),
            ref item => Err(StreamError::new(format!("expected stream, found {:?}", item), rnode))
        }
    }
}

impl Describe for Str {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .finish(prec)
    }
}

impl Stream<Char> for Str {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<Char> + 'node> {
        self.source.map_iter(Item::into_char)
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_chars_string() {
        use super::*;

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

        test_describe!("\"abc\".chars" => "\"abc\".chars");
        test_describe!("['a', 'b', 'c'].string" => "['a', 'b', 'c'].string");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("chars", Chars::eval, r#"
Splits `string` into a stream of characters.
= string.?
> "Hello".? => ['H', 'e', 'l', 'l', 'o']
> "Hello".?:ord => [8, 5, 12, 12, 15]
: string
"#);
    symbols.insert("string", Str::eval, r#"
Turns a stream of characters into a string.
* Functionally equivalent to `?cat` but optimized for this purpose.
= stream.?
> ['a', 'b', 'c'].? => "abc"
> 1.? => !not a stream
> ['a', 'b', 1].? => "ab<!> ; non-character in the stream only causes error when it's reached
: chars
: cat
: join
: numstr
"#);
}

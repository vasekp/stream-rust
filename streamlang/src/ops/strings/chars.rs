use crate::base::*;

fn eval_chars(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let stm = node.source_checked()?.to_char_stream()?;
    Ok(Item::new_stream(Chars{head: node.head, source: stm}))
}

struct Chars {
    head: Head,
    source: Rc<dyn Stream<Char>>
}

impl Describe for Chars {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .finish(prec)
    }
}

impl Stream for Chars {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator> {
        self.source.map(Item::Char)
    }

    fn len(&self) -> Length {
        self.source.len()
    }
}

fn eval_str(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let stm = node.source_checked()?.to_stream()?;
    Ok(Item::new_string(Str{head: node.head, source: stm}))
}

struct Str {
    head: Head,
    source: Rc<dyn Stream>
}

impl Describe for Str {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .finish(prec)
    }
}

impl Stream<Char> for Str {
    fn to_iter(self: Rc<Self>) -> Box<dyn SIterator<Char>> {
        Box::new(SMap::new(&self.source, Item::into_char, &self))
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
    symbols.insert("chars", eval_chars, r#"
Splits `string` into a stream of characters.
= string.?
> "Hello".? => ['H', 'e', 'l', 'l', 'o']
> "Hello".?:ord => [8, 5, 12, 12, 15]
: string
"#);
    symbols.insert("string", eval_str, r#"
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

use crate::base::*;

fn eval_ulcase(node: Node, env: &Env) -> Result<Item, StreamError> {
    try_with!(node, node.check_no_args()?);
    let node = node.eval_source(env)?;
    let func = match node.head.as_str() {
        Some("ucase") => Char::to_uppercase,
        Some("lcase") => Char::to_lowercase,
        _ => panic!("ulcase: unhandled head {:?}", node.head)
    };
    match node.source {
        Item::Char(ref ch) => Ok(Item::Char(func(ch))),
        Item::String(s) => Ok(Item::new_string(ULCase{head: node.head, source: s.into(), func})),
        ref item => Err(StreamError::new(format!("expected character or string, found {:?}", item), node))
    }
}

#[derive(Clone)]
struct ULCase {
    head: Head,
    source: BoxedStream<Char>,
    func: fn(&Char) -> Char
}

impl Describe for ULCase {
    fn describe_inner(&self, prec: u32, env: &Env) -> String {
        DescribeBuilder::new(&self.head, env)
            .set_source(&self.source)
            .finish(prec)
    }
}

impl Stream<Char> for ULCase {
    fn iter<'node>(&'node self) -> Box<dyn SIterator<Char> + 'node> {
        self.source.map_iter(|ch| Ok((self.func)(&ch)))
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
        test_eval!("['A',' ','Ch','ň','ß']:ucase" => "['A', ' ', 'CH', 'Ň', 'SS']");
        test_eval!("['A',' ','Ch','Ň','ẞ']:lcase" => "['a', ' ', 'ch', 'ň', 'ß']");
        test_eval!("\"Hello, world!\".lcase" => "\"hello, world!\"");
        test_eval!("\"Hello, world!\".ucase" => "\"HELLO, WORLD!\"");
        test_describe!("\"Hello, world!\".lcase" => "\"Hello, world!\".lcase");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("ucase", eval_ulcase);
    symbols.insert("lcase", eval_ulcase);
}

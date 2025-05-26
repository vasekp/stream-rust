use crate::base::*;

fn eval_chars(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    try_with!(node, node.check_no_args()?);
    let rnode = node.eval_all(env)?.resolve_source()?;
    match rnode.source {
        Item::String(stm) => Ok(Item::Stream(stm)),
        ref item => Err(StreamError::new(format!("expected string, found {:?}", item), rnode))
    }
}

fn eval_string(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    try_with!(node, node.check_no_args()?);
    let rnode = node.eval_all(env)?.resolve_source()?;
    match rnode.source {
        Item::Stream(stm) => Ok(Item::String(stm)),
        ref item => Err(StreamError::new(format!("expected stream, found {:?}", item), rnode))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_chars_string() {
        use crate::parser::parse;
        assert_eq!(parse("\"abc\".chars").unwrap().eval_default().unwrap().to_string(), "['a', 'b', 'c']");
        assert_eq!(parse("\"\".chars").unwrap().eval_default().unwrap().to_string(), "[]");
        assert_eq!(parse("'a'.repeat.chars").unwrap().eval_default().unwrap().to_string(), "['a', 'a', 'a', 'a', 'a', ...]");
        assert!(parse("\"abc\".chars(1)").unwrap().eval_default().is_err());
        assert!(parse("['a', 'b', 'c'].chars").unwrap().eval_default().is_err());

        assert_eq!(parse("['a', 'b', 'c'].string").unwrap().eval_default().unwrap().to_string(), "\"abc\"");
        assert_eq!(parse("[].string").unwrap().eval_default().unwrap().to_string(), "\"\"");
        assert_eq!(parse("'a'.repeat.chars.string").unwrap().eval_default().unwrap().to_string(), "\"aaaaaaaaaaaaaaaaaaaa...");
        assert_eq!(parse("['a', 'b', \"c\"].string").unwrap().eval_default().unwrap().to_string(), "\"ab<!>");
        assert_eq!(parse("seq.string").unwrap().eval_default().unwrap().to_string(), "\"<!>");
        assert_eq!(parse("range('a','c').string").unwrap().eval_default().unwrap().to_string(), "\"abc\"");
        assert!(parse("\"abc\".string").unwrap().eval_default().is_err());
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("chars", eval_chars);
    keywords.insert("string", eval_string);
}

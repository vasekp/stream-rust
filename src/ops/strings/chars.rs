use crate::base::*;

fn eval_chars(node: Node, env: &Env) -> Result<Item, StreamError> {
    try_with!(node, node.check_no_args()?);
    let rnode = node.eval_all(env)?.resolve_source()?;
    match rnode.source {
        Item::String(stm) => Ok(Item::Stream(stm)),
        ref item => Err(StreamError::new(format!("expected string, found {:?}", item), rnode))
    }
}

fn eval_string(node: Node, env: &Env) -> Result<Item, StreamError> {
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
    keywords.insert("chars", eval_chars);
    keywords.insert("string", eval_string);
}

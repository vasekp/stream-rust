use crate::base::*;

fn eval_list(node: Node, env: &Env) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source()?);
    Ok(Item::new_stream(List::from(node.args)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_list() {
        test_eval!("[1,2,3]" => "[1, 2, 3]");
        test_eval!("list(1,2,3)" => "[1, 2, 3]");
        test_eval!("list()" => "[]");
        test_eval!("[1].list(1,2,3)" => err);
        test_len!("[1,2,3]" => 3);
        test_len!("[1]" => 1);
        test_len!("[]" => 0);
        test_advance("[1,2,3]");
        test_advance("[1]");
        test_advance("[]");
        test_describe!("[1,2,3]" => "[1, 2, 3]");
        test_describe!("[]" => "[]");
        test_describe!("list(1)" => "[1]");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert_with_docs(["list", "*list"], eval_list, r#"
A stream formed by `?`'s arguments.
= ?(item1, item2, ...)
> list(1, 2, 3) => [1, 2, 3]
"#);
}

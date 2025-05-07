use crate::base::*;

fn eval_list(node: Node, env: &Rc<Env>) -> Result<Item, StreamError> {
    let node = node.eval_all(env)?;
    try_with!(node, node.check_no_source()?);
    Ok(Item::new_stream(List::from(node.args)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_list() {
        use crate::parser::parse;
        assert_eq!(parse("[1,2,3]").unwrap().eval_default().unwrap().to_string(), "[1, 2, 3]");
        test_len_exact(&parse("[1,2,3]").unwrap().eval_default().unwrap(), 3);
        test_len_exact(&parse("[1]").unwrap().eval_default().unwrap(), 1);
        test_len_exact(&parse("[]").unwrap().eval_default().unwrap(), 0);
        test_skip_n(&parse("[1,2,3]").unwrap().eval_default().unwrap());
        test_skip_n(&parse("[1]").unwrap().eval_default().unwrap());
        test_skip_n(&parse("[]").unwrap().eval_default().unwrap());
        assert_eq!(parse("[1,2,3]").unwrap().eval_default().unwrap().describe(), "[1, 2, 3]");
        assert_eq!(parse("[]").unwrap().eval_default().unwrap().describe(), "[]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("*list", eval_list);
}

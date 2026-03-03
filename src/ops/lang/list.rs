use crate::base::*;

fn eval_list(node: &Node, env: &Env) -> Result<Item, StreamError> {
    node.check_no_source()?;
    let list = node.args.iter()
        .map(|expr| expr.eval(env))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Item::new_stream(List::from(list)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_list() {
        test_eval!("[1,2,3]" => "[1, 2, 3]");
        test_len!("[1,2,3]" => 3);
        test_len!("[1]" => 1);
        test_len!("[]" => 0);
        test_advance("[1,2,3]");
        test_advance("[1]");
        test_advance("[]");
        test_describe!("[1,2,3]" => "[1, 2, 3]");
        test_describe!("[]" => "[]");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert_raw("[list]", eval_list);
}

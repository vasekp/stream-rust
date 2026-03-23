use crate::base::*;

fn eval_digits(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_source()?;
    let base = match &node.args[..] {
        [] => 10,
        [Item::Number(num)] => num.try_cast_within(2..=36)?,
        _ => return Err(StreamError::usage(&node.head)),
    };
    let vec = ('0'..='9').chain('A'..='Z')
        .take(base)
        .map(Item::new_char)
        .collect::<Vec<_>>();
    Ok(Item::new_stream(List::from(vec)))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_digits() {
        test_eval!("digits" : 10 => "['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']");
        test_eval!("digits(6)" : 10 => "['0', '1', '2', '3', '4', '5']");
        test_eval!("digits(2)" : 10 => "['0', '1']");
        test_eval!("digits(1)" => err);
        test_eval!("digits(16)" : 20 => "['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']");
        test_eval!("digits(26).last(5)" => "['L', 'M', 'N', 'O', 'P']");
        test_eval!("digits(36).last(5)" => "['V', 'W', 'X', 'Y', 'Z']");
        test_eval!("digits(37)" => err);
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("digits", eval_digits, r#"
The list of digits in base `base` as characters.
If `base` is not specified, it defaults to 10. If it exceeds 10, uppercase letters are used.
= ?
= ?(base)
> ? : 10 => ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
> ?(16) : 20 => ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']
: alpha
"#);
}

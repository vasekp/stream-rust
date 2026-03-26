use crate::base::*;

fn eval_isin(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let item = node.source_checked()?;
    let stm = node.only_arg_checked()?.as_stream()?;
    for elm in stm.iter().transposed() {
        check_stop!();
        if item.try_eq(&elm?)? {
            return Ok(Item::Bool(true));
        }
    }
    Ok(Item::Bool(false))
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_isin() {
        use super::*;
        test_eval!("(1..5):isin([1, 5, 7])" => "[true, false, false, false, true]");
        test_eval!("(1..5):isin([])" => "[false, false, false, false, false]");
        test_eval!("(1..5):isin([1, 1, 1])" => "[true, false, false, false, false]");
        test_eval!("(1..5).isin([1, 5, 7])" => "false");
        test_eval!("'a'.isin(\"abc\")" => err);
        test_eval!("\"one\".isin([\"one\", \"two\", \"three\"])" => "true");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["isin", "in", "element"], eval_isin, r#"
Evaluates to `true` if `item` appears in `stream`, `false` otherwise.
= item.?(stream)
> (1..5):?([4, 5, 6]) => [false, false, false, true, true]
> "12 35 -1 0".?consec{#.?(?digits~'+'~'-')}:?strnum => [12, 35, -1, 0]
: contains
: isalpha
: isdigit
"#);
}

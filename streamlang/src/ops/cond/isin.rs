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

fn eval_iswithin(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    match (node.source_checked()?, &node.args[..]) {
        (Item::Number(x), [Item::Number(min), Item::Number(max)])
            => Ok(Item::Bool(x >= min && x <= max)),
        (Item::Char(x), [Item::Char(min), Item::Char(max)]) => {
            let ox = env.alpha.ord(x)?;
            let omin = env.alpha.ord(min)?;
            let omax = env.alpha.ord(max)?;
            Ok(Item::Bool(ox >= omin && ox <= omax))
        },
        _ => Err(StreamError::usage(&node.head)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_isin() {
        test_eval!("(1..5):isin([1, 5, 7])" => "[true, false, false, false, true]");
        test_eval!("(1..5):isin([])" => "[false, false, false, false, false]");
        test_eval!("(1..5):isin([1, 1, 1])" => "[true, false, false, false, false]");
        test_eval!("(1..5).isin([1, 5, 7])" => "false");
        test_eval!("'a'.isin(\"abc\")" => err);
        test_eval!("\"one\".isin([\"one\", \"two\", \"three\"])" => "true");
    }

    #[test]
    fn test_iswithin() {
        // in addition to doc-tests
        test_eval!("'h'.iswithin('a', 'm')" => "true");
        test_eval!("'h'.iswithin('m', 'a')" => "false");
        test_eval!("'m'.iswithin('m', 'a')" => "false");
        test_eval!("'a'.iswithin('m', 'a')" => "false");
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
: iswithin
"#);
    symbols.insert("iswithin", eval_iswithin, r#"
Evaluates to `true` if `x` lies in a range given by `min` and `max`.
All three values may (simultaneously) be numbers or characters.
= x.?(min, max)
> (1..5):?(2, 4) => [false, true, true, true, false]
> 'e'.?('a', 'h') => true
> '5'.?('0', '9') => !not in alphabet ; character ranges only work in a given alphabet
: isin
: clamp
"#);
}

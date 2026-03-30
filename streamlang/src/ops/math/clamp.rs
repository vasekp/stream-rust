use crate::base::*;

fn eval_floor(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let x = node.source_checked()?.to_num()?;
    let y = node.only_arg_checked()?.to_num()?;
    Ok(Item::Number(std::cmp::max(x, y)))
}

fn eval_ceil(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let x = node.source_checked()?.to_num()?;
    let y = node.only_arg_checked()?.to_num()?;
    Ok(Item::Number(std::cmp::min(x, y)))
}

fn eval_clamp(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let x = node.source_checked()?.to_num()?;
    let [Item::Number(min), Item::Number(max)] = &node.args[..] else {
        return Err(StreamError::usage(&node.head));
    };
    if min > max {
        Err("expected: min ≤ max".into())
    } else if &x < min {
        Ok(Item::Number(min.to_owned()))
    } else if &x > max {
        Ok(Item::Number(max.to_owned()))
    } else {
        Ok(Item::Number(x.to_owned()))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_clamp() {
        use super::*;
        test_eval!("[5, 1, 7, 3]:floor(4)" => "[5, 4, 7, 4]");
        test_eval!("[5, 1, 7, 3]:ceil(4)" => "[4, 1, 4, 3]");
        test_eval!("[5, 1, 7, 3]:clamp(2, 5)" => "[5, 2, 5, 3]");
        test_eval!("[5, 1, 7, 3]:clamp(5, 5)" => "[5, 5, 5, 5]");
        test_eval!("3.clamp(2,5)" => "3");
        test_eval!("2.clamp(2,5)" => "2");
        test_eval!("5.clamp(2,5)" => "5");
        test_eval!("3.clamp(5,2)" => err);
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("floor", eval_floor, r#"
If number `x` is smaller than `min`, evaluates to `min`, otherwise to `x`.
= x.?(min)
> (1..5):?(3) => [3, 3, 3, 4, 5]
: ceiling
: clamp
: min
: max
"#);
    symbols.insert(["ceiling", "ceil"], eval_ceil, r#"
If number `x` is greater than `max`, evaluates to `max`, otherwise to `x`.
= x.?(max)
> (1..5):?(3) => [1, 2, 3, 3, 3]
: floor
: clamp
: min
: max
"#);
    symbols.insert("clamp", eval_clamp, r#"
Restricts number `x` to within `min` and `max` (inclusive).
= x.?(min, max)
> (1..5):?(2, 4) => [2, 2, 3, 4, 4]
: floor
: ceiling
: iswithin
: min
: max
"#);
}

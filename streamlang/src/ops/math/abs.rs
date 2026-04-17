use crate::base::*;

fn eval_abs(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let num = node.source_checked()?.as_num()?;
    Ok(Item::Number(num.abs()))
}

fn eval_sign(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    node.check_no_args()?;
    let num = node.source_checked()?.as_num()?;
    Ok(Item::Number(num.signum()))
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("abs", eval_abs, r#"
Absolute value of `number`.
= number.?
> (-2..2):? => [2, 1, 0, 1, 2]
: sign
"#);
    symbols.insert("sign", eval_sign, r#"
Sign of `number`, i.e., `-1`, `0`, or `1`.
= number.?
> (-2..2):? => [-1, -1, 0, 1, 1]
: abs
"#);
}

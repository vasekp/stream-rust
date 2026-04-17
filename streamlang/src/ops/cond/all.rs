use crate::base::*;

fn eval_all(node: &Node, env: &Env) -> SResult<Item> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let cond = node.only_arg_checked()?.as_func()?;
    for item in stm.iter().transposed() {
        check_stop!();
        let val = cond.with_source(item?.into())?.eval(env)?.to_bool()?;
        if !val { return Ok(Item::Bool(false)); }
    }
    Ok(Item::Bool(true))
}

fn eval_some(node: &Node, env: &Env) -> SResult<Item> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let cond = node.only_arg_checked()?.as_func()?;
    for item in stm.iter().transposed() {
        check_stop!();
        let val = cond.with_source(item?.into())?.eval(env)?.to_bool()?;
        if val { return Ok(Item::Bool(true)); }
    }
    Ok(Item::Bool(false))
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_all_some() {
        use super::*;
        test_eval!("(1..3).all{#>0}" => "true");
        test_eval!("(-3..3).all{#>0}" => "false");
        test_eval!("(-3..-1).all{#>0}" => "false");
        test_eval!("[1, 0, 'a'].all{#>0}" => "false");
        test_eval!("(1..3).some{#>0}" => "true");
        test_eval!("(-3..3).some{#>0}" => "true");
        test_eval!("(-3..-1).some{#>0}" => "false");
        test_eval!("[1, 0, 'a'].some{#>0}" => "true");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert(["all", "each"], eval_all, r#"
Evaluates to `true` if `x.cond` is `true` for every item `x` of `stream`, `false` otherwise.
= stream.?{cond}
> [1, 2, -1, 0, 5].?{# > 0} => false
> "hello".?chars.?(?isascii) => true
: some
"#);
    symbols.insert(["some", "any"], eval_some, r#"
Evaluates to `true` if `x.cond` is `true` for at least one item `x` of `stream`, `false` otherwise.
= stream.?{cond}
> [1, 2, -1, 0, 5].?{# > 0} => true
> "hello".?chars.?(?isupper) => false
: all
"#);
}

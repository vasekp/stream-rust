use crate::base::*;

fn eval_reduce(node: &Node, env: &Env) -> SResult<Item> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let func = if let [Expr::Eval(body)] = &node.args[..] && body.args.is_empty() {
        body
    } else {
        return Err(StreamError::usage(&node.head));
    };
    let mut iter = stm.iter();
    let Some(mut val) = iter.next()? else {
        return Err("stream is empty".into());
    };
    for res in iter.transposed() {
        check_stop!();
        val = func.with_args(vec![Expr::from(val), Expr::from(res?)])?.eval(env)?;
    }
    Ok(val)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_reduce() {
        use super::*;
        test_eval!("(1..5).reduce(plus)" => "15");
        test_eval!("(1..5).reduce(times)" => "120");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("reduce", eval_reduce, r#"
Merges all elements of `stream` using `func` as `func(func(x1, x2), x3)` etc.
= stream.?{func}
> (1..5).?(?plus) => 15 ; also see ?total
> ?range(10,1,-2).?(?times) => 3840 ; double factorial
: fold
: cat
"#);
}

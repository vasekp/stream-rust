use crate::base::*;

fn eval_minmax(node: &Node, env: &Env) -> SResult<Item> {
    let src = node.source.as_ref().map(|expr| expr.eval(env)).transpose()?;
    match (src, &node.args[..]) {
        (Some(Item::Stream(_)), []) | (None, [_, ..]) => {
            let node = node.eval_all(env)?;
            let cmp = match &node.head.as_str().unwrap()[0..3] {
                "min" => std::cmp::min,
                "max" => std::cmp::max,
                _ => unreachable!(),
            };
            eval_fn(&node, cmp).map(Item::Number)
        },
        (Some(Item::Stream(stm)), [expr]) => {
            let func = expr.as_func()?;
            let cmp = match &node.head.as_str().unwrap()[0..3] {
                "min" => PartialOrd::lt,
                "max" => PartialOrd::gt,
                _ => unreachable!(),
            };
            eval_mapped(&stm, func, cmp, env)
        },
        _ => Err(StreamError::usage(&node.head))
    }
}

fn eval_fn(node: &Node<Item>, func: fn(Number, Number) -> Number) -> SResult<Number> {
    if let Some(Item::Stream(stm)) = &node.source && node.args.is_empty() {
        let mut iter = stm.iter();
        let Some(first) = iter.next()? else {
            return Err("stream is empty".into());
        };
        let mut x = first.to_num()?;
        for res in iter.transposed() {
            check_stop!();
            x = func(x, res?.to_num()?);
        }
        Ok(x)
    } else if node.source.is_none() && !node.args.is_empty() {
        let mut iter = node.args.iter();
        let mut x = iter.next().unwrap().to_num()?; // just checked
        for item in iter {
            x = func(x, item.to_num()?);
        }
        Ok(x)
    } else {
        Err(StreamError::usage(&node.head))
    }
}

fn eval_mapped(stm: &Rc<dyn Stream>, func: &Rc<Node>,
    cmp: fn(&Number, &Number) -> bool, env: &Env) -> SResult<Item> {
    let mut iter = stm.iter();
    let Some(mut val) = iter.next()? else {
        return Err("stream is empty".into());
    };
    let mut key = func.with_source(Expr::from(&val))?.eval(env)?.to_num()?;
    for res in iter.transposed() {
        check_stop!();
        let item = res?;
        let x = func.with_source(Expr::from(&item))?.eval(env)?.to_num()?;
        if cmp(&x, &key) {
            key = x;
            val = item;
        }
    }
    Ok(val)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_minmax() {
        use super::*;
        test_eval!("[5, 1, 7, 3].min" => "1");
        test_eval!("[5, 1, 7, 3].max" => "7");
        test_eval!("[5, 1, 7, 'a'].max" => err);
        test_eval!("(3..5):{1..#}.min(len)" => "[1, 2, 3]");
        test_eval!("(3..5):{1..#}.max(len)" => "[1, 2, 3, 4, 5]");
        test_eval!("\"hello\".chars.min(ord)" => "'e'");
        test_eval!("\"hello\".chars.max(ord)" => "'o'");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("min", eval_minmax, r#"
Minimum of an input stream of numbers, or of given arguments.
If `func` is provided, compares `x.func` instead of `x` itself.
= stream.?
= ?(number, number, ...)
= stream.?(func)
> ?(5, 10, 3) => 3
> (3..6).? => 3
> [[5, 7], [2, 5], [3, 3]].?(?first) => [2, 5]
> "hello".?chars.?(?ord) => 'e'
> [-2, 3, -5].?(?abs) => -2
> [5, 7, 8, 1, 2, 3].?enum.?(first) => [1, 4] ; minimum along with its index
: max
: sort
"#);
    symbols.insert("max", eval_minmax, r#"
Maximum of an input stream of numbers, or of given arguments.
If `func` is provided, compares `x.func` instead of `x` itself.
= stream.?
= ?(number, number, ...)
= stream.?(func)
> ?(5, 10, 3) => 10
> (3..6).? => 6
> [[5, 7], [2, 5], [3, 3]].?(?first) => [5, 7]
> "hello".?chars.?(?ord) => 'o'
> [-2, 3, -5].?(?abs) => -5
> [5, 7, 8, 1, 2, 3].?enum.?(first) => [8, 3] ; maximum along with its index
: min
: sort
"#);
}

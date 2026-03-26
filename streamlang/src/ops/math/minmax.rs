use crate::base::*;

fn eval_min(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    eval_fn(&node, std::cmp::min).map(Item::Number)
}

fn eval_max(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    eval_fn(&node, std::cmp::max).map(Item::Number)
}

fn eval_minby(node: &Node, env: &Env) -> SResult<Item> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let func = node.only_arg_checked()?.as_func()?;
    eval_mapped(&stm, func, PartialOrd::lt, env)
}

fn eval_maxby(node: &Node, env: &Env) -> SResult<Item> {
    let stm = node.source_checked()?.eval(env)?.to_stream()?;
    let func = node.only_arg_checked()?.as_func()?;
    eval_mapped(&stm, func, PartialOrd::gt, env)
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
        test_eval!("(3..5):{1..#}.minby(len)" => "[1, 2, 3]");
        test_eval!("(3..5):{1..#}.maxby(len)" => "[1, 2, 3, 4, 5]");
        test_eval!("\"hello\".chars.minby(ord)" => "'e'");
        test_eval!("\"hello\".chars.maxby(ord)" => "'o'");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("min", eval_min, r#"
Minimum of an input stream of numbers, or of given arguments.
= stream.?
= ?(number, number, ...)
> ?(5, 10, 3) => 3
> (3..6).? => 3
: max
: minby
: sort
"#);
    symbols.insert("max", eval_max, r#"
Maximum of an input stream of numbers, or of given arguments.
= stream.?
= ?(number, number, ...)
> ?(5, 10, 3) => 10
> (3..6).? => 6
: min
: maxby
: sort
"#);
    symbols.insert("minby", eval_minby, r#"
Finds and returns the element `x` of the input `stream` for which `x.func` is smallest.
= stream.?(func)
> [[5, 7], [2, 5], [3, 3]].?(?first) => [2, 5]
> "hello".?chars.?(?ord) => 'e'
> [-2, 3, -5].?(?abs) => -2
> [5, 7, 8, 1, 2, 3].?enum.?(first) => [1, 4] ; minimum along with its index
: min
: maxby
: sortby
"#);
    symbols.insert("maxby", eval_maxby, r#"
Finds and returns the element `x` of the input `stream` for which `x.func` is largest.
= stream.?(func)
> [[5, 7], [2, 5], [3, 3]].?(?first) => [5, 7]
> "hello".?chars.?(?ord) => 'o'
> [-2, 3, -5].?(?abs) => -5
> [5, 7, 8, 1, 2, 3].?enum.?(first) => [8, 3] ; maximum along with its index
: max
: minby
: sortby
"#);
}

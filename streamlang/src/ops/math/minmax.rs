use crate::base::*;

fn eval_min(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    eval_fn(&node, std::cmp::min).map(Item::Number)
}

fn eval_max(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    eval_fn(&node, std::cmp::max).map(Item::Number)
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


#[cfg(test)]
mod tests {
    #[test]
    fn test_gcd() {
    }

}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("min", eval_min, r#"
Minimum of an input stream of numbers, or of given arguments.
= stream.?
= ?(number, number, ...)
> ?(5, 10, 3) => 3
? (3..6).? => 3
: max
"#);
    symbols.insert("max", eval_max, r#"
Maximum of an input stream of numbers, or of given arguments.
= stream.?
= ?(number, number, ...)
> ?(5, 10, 3) => 10
? (3..6).? => 6
: min
"#);

}

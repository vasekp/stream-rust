use crate::base::*;

use std::cmp::Ordering;

fn eval_minmax(node: &Node, env: &Env) -> SResult<Item> {
    // A new item replaces the current best if it compares to it as `keep`.
    let keep = match &node.head.as_str().unwrap()[0..3] {
        "min" => Ordering::Less,
        "max" => Ordering::Greater,
        _ => unreachable!(),
    };
    let source = node.source.as_ref().map(|expr| expr.eval(env)).transpose()?;
    match (source, &node.args[..]) {
        (Some(Item::Stream(stm)), []) => eval_items(stm.iter().transposed(), keep),
        (None, [_, ..]) => eval_items(node.args.iter().map(|expr| expr.eval(env)), keep),
        (Some(Item::Stream(stm)), [expr]) => eval_mapped(&stm, &Function::new(expr.as_func()?, env), keep),
        _ => Err(StreamError::usage(&node.head)),
    }
}

fn eval_items(mut items: impl Iterator<Item = SResult<Item>>, keep: Ordering) -> SResult<Item> {
    let Some(first) = items.next() else {
        return Err("stream is empty".into());
    };
    let mut best = first?.into_num()?;
    for item in items {
        check_stop!();
        let x = item?.into_num()?;
        if x.cmp(&best) == keep {
            best = x;
        }
    }
    Ok(Item::Number(best))
}

fn eval_mapped(stm: &Rc<dyn Stream>, func: &Function, keep: Ordering) -> SResult<Item> {
    let mut iter = stm.iter();
    let Some(mut val) = iter.next()? else {
        return Err("stream is empty".into());
    };
    let mut key = func.call_source(val.clone())?.into_num()?;
    for res in iter.transposed() {
        check_stop!();
        let item = res?;
        let x = func.call_source(item.clone())?.into_num()?;
        if x.cmp(&key) == keep {
            key = x;
            val = item;
        }
    }
    Ok(val)
}

#[rustfmt::skip]
pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("min", eval_minmax, r#"
Minimum of an input stream of numbers, or of given arguments.
If `func` is provided, compares `x.func` instead of `x` itself.
* Ties return the first item. An empty stream is an error.
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
* Ties return the first item. An empty stream is an error.
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
        test_eval!("min(5, 1, 7, 3)" => "1");
        test_eval!("max(5, 1, 7, 3)" => "7");
        test_eval!("max(5, 1, 'a')" => err);
        test_eval!("[].min" => err);
        test_eval!("[].min(abs)" => err);
        test_eval!("5.min" => err);
        test_eval!("[1].min(1)" => err);
        test_eval!("[1].min(abs, abs)" => err);
        test_eval!("min()" => err);
        test_eval!("[[1]].min(len).min(abs)" => "1");
        // ties return the first item
        test_eval!("[[5, 1], [2, 9], [5, 2], [2, 8]].max(first)" => "[5, 1]");
        test_eval!("[[5, 1], [2, 9], [5, 2], [2, 8]].min(first)" => "[2, 9]");
    }
}

use crate::base::*;

fn eval_dot(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let (s1, s2) = match (&node.source, &node.args[..]) {
        (Some(Item::Stream(s1)), [Item::Stream(s2)]) => (s1, s2),
        (None, [Item::Stream(s1), Item::Stream(s2)]) => (s1, s2),
        _ => return Err(StreamError::usage(&node.head)),
    };
    let mut it1 = s1.iter();
    let mut it2 = s2.iter();
    let mut total = Number::zero();
    loop {
        check_stop!();
        match (it1.next()?, it2.next()?) {
            (Some(x), Some(y)) => total += x.as_num()? * y.as_num()?,
            (None, None) => break,
            _ => return Err("streams of inequal lengths".into()),
        }
    }
    Ok(Item::new_number(total))
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_dot() {
        use super::*;
        test_eval!("[1,2,3].dot([4,5,6])" => "32");
        test_eval!("[].dot([])" => "0");
        test_eval!("[1].dot([4,5,6])" => err);
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("dot", eval_dot, r#"
Evaluates the dot product of two streams of numbers.
= stream.?(stream)
= ?(stream, stream)
> [10, 1].?([4, 5]) => 45
> [1, 2].?([[4, 5], [6, 7]]) => !expected number ; only usable for streams of numbers
> [1, 2].?zip([[4, 5], [6, 7]]):{?times@#}.{?plus@#} => [16, 19] ; use a more verbose construction for lists of lists etc.
> [1, 10, 100].?(?seq) => !streams of inequal lengths
> [1, 10, 100].?zip(?seq):{?times@#}.?total => 321 ; ditto
"#);
}

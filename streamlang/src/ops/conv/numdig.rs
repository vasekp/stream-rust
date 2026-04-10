use crate::base::*;
use super::digits::Digits;

fn eval_numdig(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let num = node.source_checked()?.to_num()?;
    let (radix, minw) = match &node.args[..] {
        [] => (10, None),
        [Item::Number(radix)] => (radix.try_cast_within(2u32..)?, None),
        [Item::Number(radix), Item::Number(minw)] => (radix.try_cast_within(2u32..)?, Some(minw.try_unsign()?)),
        _ => return Err(StreamError::usage(&node.head))
    };
    let digits = Digits::new(num.try_unsign()?, radix)
        .map(Item::new_number)
        .collect::<Vec<_>>();
    if let Some(minw) = minw {
        Expr::from(Item::from(digits)).chain(Link::new("padleft",
                vec![Expr::new_number(minw), Expr::new_number(0)]))
            .eval(env)
    } else {
        Ok(Item::from(digits))
    }
}

fn eval_dignum(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let stm = node.source_checked()?.as_stream()?;
    let radix = match &node.args[..] {
        [] => 10,
        [Item::Number(radix)] => radix.try_cast_within(2u32..)?,
        _ => return Err(StreamError::usage(&node.head))
    };
    let vec = stm.listout_with(|item| item.into_num()?.try_cast_within(0..radix))?;
    if vec.is_empty() {
        return Err("stream is empty".into());
    }
    let mut num = UNumber::zero();
    for digit in vec {
        num *= radix;
        num += digit;
    }
    Ok(Item::new_number(num))
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_numdig() {
        use super::*;
        test_eval!("(2^100).numdig" => "[1, 2, 6, 7, 6, ...]");
        test_eval!("(-15).numdig" => err);
        test_eval!("0.numdig" => "[0]");
        test_eval!("42.numdig(10, 5)" => "[0, 0, 0, 4, 2]");
        test_eval!("65535.numdig(10, 5)" => "[6, 5, 5, 3, 5]");
        test_eval!("65535.numdig(10, 3)" => "[6, 5, 5, 3, 5]");
        test_eval!("(16^20-2).numdig(16)" => "[15, 15, 15, 15, 15, ...]");
        test_eval!("42.numdig(2)" => "[1, 0, 1, 0, 1, ...]");
        test_eval!("42.numdig(36)" => "[1, 6]");
        test_eval!("42.numdig(37)" => "[1, 5]");
        test_eval!("42.numdig(0)" => err);
        test_eval!("42.numdig(1)" => err);
        test_eval!("42.numdig(-1)" => err);
        test_eval!("65535.numdig(255)" => "[1, 2, 0]");
        test_eval!("65535.numdig(256)" => "[255, 255]");
        test_eval!("65535.numdig(257)" => "[255, 0]");
        test_eval!("(10^10).numdig(10^9)" => "[10, 0]");
    }

    #[test]
    fn test_dignum() {
        use super::*;
        test_eval!("[1, 6].dignum" => "16");
        test_eval!("[0, 0].dignum" => "0");
        test_eval!("[10].dignum" => err);
        test_eval!("[-1].dignum" => err);
        test_eval!("[].dignum" => err);
        test_eval!("[1, 6].dignum(36)" => "42");
        test_eval!("[1, 6].dignum(255)" => "261");
        test_eval!("[255, 255].dignum(256)" => "65535");
        test_eval!("[1, 0].dignum(257)" => "257");
        test_eval!("[1, 0].dignum(10^9)" => "1000000000");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("numdig", eval_numdig, r#"
Converts `number` to a stream of digits (most significant first).
If `base` is omitted, it defaults to 10 (decadic).
If `min_width` is given, the stream is zero-padded if shorter.
! `?` can not accept negative numbers.
= number.?
= number.?(base)
= number.?(base, min_width)
> 42.? => [4, 2]
> 42.?(10, 5) => [0, 0, 0, 4, 2]
> 123456789.?(1000) => [123, 456, 789]
: dignum
: numstr
: digstr
"#);
    symbols.insert("dignum", eval_dignum, r#"
Converts a stream of digits into a number.
If `base` is omitted, it defaults to 10 (decadic).
= stream.?
= stream.?(base)
> [1, 6].? => 16
> [1, 0, 0, 0].?(2) => 8
: numdig
: strnum
: digstr
"#);
}

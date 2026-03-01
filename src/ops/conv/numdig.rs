use crate::base::*;
use super::digits::Digits;

fn eval_numdig(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    let (num, radix, minw) = match &rnode {
        RNodeS { source: Item::Number(num), args: RArgs::Zero, .. } => (num, 10, None),
        RNodeS { source: Item::Number(num), args: RArgs::One(Item::Number(radix)), .. } =>
            try_with!(rnode, (num, check_radix(radix)?, None)),
        RNodeS { source: Item::Number(num), args: RArgs::Two(Item::Number(radix), Item::Number(minw)), .. }
        if !minw.is_negative() =>
            try_with!(rnode, (num, check_radix(radix)?, Some(crate::utils::unsign(minw.clone())))),
        _ => return Err(StreamError::new("expected: number.numdig or number.numdig(radix) or \
number.numdig(radix, min_width)", rnode))
    };
    if num.is_negative() {
        return Err(StreamError::new("can only accept nonnegative numbers", rnode));
    }
    let digits = Digits::new(num.unsigned_abs(), radix as u32)
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

fn eval_dignum(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    let (s, radix) = match &rnode {
        RNodeS { source: Item::Stream(s), args: RArgs::Zero, .. } => (s, 10),
        RNodeS { source: Item::Stream(s), args: RArgs::One(Item::Number(radix)), .. } =>
            try_with!(rnode, (s, check_radix(radix)?)),
        _ => return Err(StreamError::new("expected: stream.dignum or stream.dignum(radix) or \
stream.dignum(radix, min_width)", rnode))
    };
    if s.is_empty() {
        return Err(StreamError::new("stream is empty", rnode));
    }
    let vec = try_with!(rnode, s.iter().map(|item| {
            check_stop!();
            item?.into_num()?
                .try_into()
                .map_err(|_| BaseError::from("invalid digit"))
        })
        .collect::<Result<Vec<u32>, _>>()?);
    let mut num = UNumber::zero();
    for digit in vec {
        if !(0..radix).contains(&digit) {
            return Err(StreamError::new("invalid digit", rnode));
        }
        num *= radix;
        num += digit;
    }
    Ok(Item::new_number(num))
}

pub(crate) fn check_radix(radix: &Number) -> Result<u32, BaseError> {
    if radix < &Number::from(2) {
        Err("base must be at least 2".into())
    } else {
        match radix.try_into() {
            Ok(radix) => Ok(radix),
            _ => Err("base too large".into())
        }
    }
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
"#);
}

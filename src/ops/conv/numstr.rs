use crate::base::*;

fn eval_numstr(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    let (num, radix, minw) = match &rnode {
        RNodeS { source: Item::Number(num), args: RArgs::Zero, .. } => (num, 10, None),
        RNodeS { source: Item::Number(num), args: RArgs::One(Item::Number(radix)), .. } =>
            try_with!(rnode, (num, check_radix(radix)?, None)),
        RNodeS { source: Item::Number(num), args: RArgs::Two(Item::Number(radix), Item::Number(minw)), .. }
        if !minw.is_negative() =>
            try_with!(rnode, (num, check_radix(radix)?, Some(crate::utils::unsign(minw.clone())))),
        _ => return Err(StreamError::new("expected: number.numstr or number.numstr(radix) or \
number.numstr(radix, min_width)", rnode))
    };
    if radix == 10 && minw.is_none() {
        Ok(Item::new_string(LiteralString::from(format!("{}", num).as_str())))
    } else {
        let (sign, digits) = num.to_radix_be(radix);
        let s = std::str::from_utf8(&digits.iter()
            .map(|dig| match dig {
                0..=9 => b'0' + dig,
                10..=35 => b'A' + (dig - 10),
                _ => unreachable!()
            })
            .collect::<Vec<_>>())
            .expect("should contain only valid ASCII by construction")
            .to_owned();
        let sign = sign == num::bigint::Sign::Minus;
        match (sign, minw) {
            (false, None) => Ok(Item::new_string(LiteralString::from(s.as_str()))),
            (true, None) => Ok(Item::new_string(LiteralString::from(format!("-{s}").as_str()))),
            (false, Some(minw)) =>
                Expr::from(Item::new_string(LiteralString::from(s.as_str())))
                    .chain(Link::new("padleft", vec![Expr::new_number(minw), Expr::new_char('0')]))
                    .eval(env),
            (true, Some(minw)) =>
                Expr::new_node("join", vec![
                    Expr::new_char('-'),
                    Expr::from(Item::new_string(LiteralString::from(s.as_str())))
                        .chain(Link::new("padleft", vec![Expr::new_number(minw), Expr::new_char('0')]))
                ]).eval(env),
        }
    }
}

fn eval_strnum(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    let (s, radix) = match &rnode {
        RNodeS { source: Item::String(s), args: RArgs::Zero, .. } => (s, 10),
        RNodeS { source: Item::String(s), args: RArgs::One(Item::Number(radix)), .. } =>
            match radix.to_u32() {
                Some(radix) if (2..=36).contains(&radix) => (s, radix),
                _ => return Err(StreamError::new("radix must be between 2 and 36", rnode))
            },
        _ => return Err(StreamError::new("expected: string.strnum or string.strnum(radix)", rnode))
    };
    let st = try_with!(rnode, s.iter().map(|ch| -> Result<u8, BaseError> {
        check_stop!();
        match ch? {
            Char::Single(c) if c.is_ascii() && (c.is_digit(radix) || c == '-') => Ok(c as u8),
            _ => Err(BaseError::from("invalid character"))
        }})
        .collect::<Result<Vec<_>, _>>()?);
    let num = Number::parse_bytes(&st, radix)
        .ok_or_else(|| StreamError::new("invalid input", rnode))?;
    Ok(Item::Number(num))
}

pub(crate) fn check_radix(radix: &Number) -> Result<u32, BaseError> {
    match radix.to_u32() {
        Some(radix) if (2..=36).contains(&radix) => Ok(radix),
        _ => Err("radix must be between 2 and 256".into())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_numstr() {
        use super::*;
        test_eval!("(2^100).numstr" => "\"12676506002282294014...");
        test_eval!("(-15).numstr" => "\"-15\"");
        test_eval!("(16^20-2).numstr(16)" => "\"FFFFFFFFFFFFFFFFFFFE\"");
        test_eval!("42.numstr(2)" => "\"101010\"");
        test_eval!("0.numstr(2)" => "\"0\"");
        test_eval!("42.numstr(10, 5)" => "\"00042\"");
        test_eval!("65535.numstr(10, 5)" => "\"65535\"");
        test_eval!("65535.numstr(10, 3)" => "\"65535\"");
        test_eval!("(-2).numstr(2)" => "\"-10\"");
        test_eval!("(-2).numstr(10, 3)" => "\"-002\"");
        test_eval!("42.numstr(36)" => "\"16\"");
        test_eval!("42.numstr(37)" => err);
        test_eval!("42.numstr(0)" => err);
        test_eval!("42.numstr(1)" => err);
        test_eval!("42.numstr(-1)" => err);
    }

    #[test]
    fn test_strnum() {
        use super::*;
        test_eval!("\"-123\".strnum" => "-123");
        test_eval!("\"\".strnum" => err);
        test_eval!("\"-\".strnum" => err);
        test_eval!("\"-0\".strnum" => "0");
        test_eval!("\"-FFFFFFFFFFFFFFFFFFFF\".strnum(16)" => "-1208925819614629174706175");
        test_eval!("\"A\".strnum" => err);
        test_eval!("\"A\".strnum(11)" => "10");
        test_eval!("\"B\".strnum(11)" => err);
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("numstr", eval_numstr);
    symbols.insert("strnum", eval_strnum);
}

use crate::base::*;
use super::digits::Digits;

fn eval_numstr(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let num = node.source_checked()?.to_num()?;
    let (radix, minw) = match &node.args[..] {
        [] => (10, None),
        [Item::Number(radix)] => (radix.try_cast_within(2..=36)?, None),
        [Item::Number(radix), Item::Number(minw)] => (radix.try_cast_within(2..=36u32)?, Some(minw.try_unsign()?)),
        _ => return Err(StreamError::usage(&node.head))
    };
    if radix == 10 && minw.is_none() {
        Ok(Item::new_string(LiteralString::from(format!("{}", num).as_str())))
    } else {
        let sign = num.signum().is_negative();
        let s = Digits::new(num.unsigned_abs(), radix as u8)
            .map(|dig| (match dig {
                0..=9 => b'0' + dig,
                10..=35 => b'A' + (dig - 10),
                _ => unreachable!()
            }) as char)
            .collect::<String>();
        match (sign, minw) {
            (false, None) => Ok(Item::new_string(LiteralString::from(s.as_str()))),
            (true, None) => Ok(Item::new_string(LiteralString::from(format!("-{s}").as_str()))),
            (false, Some(minw)) =>
                Expr::from(Item::new_string(LiteralString::from(s.as_str())))
                    .chain(Link::new("padleft", vec![Expr::new_number(minw), Expr::new_char('0')]))
                    .eval(env),
            (true, Some(minw)) =>
                Expr::new_node("join", None, vec![
                    Expr::new_char('-'),
                    Expr::from(Item::new_string(LiteralString::from(s.as_str())))
                        .chain(Link::new("padleft", vec![Expr::new_number(minw), Expr::new_char('0')]))
                ]).eval(env),
        }
    }
}

fn eval_strnum(node: &Node, env: &Env) -> SResult<Item> {
    let node = node.eval_all(env)?;
    let stm = node.source_checked()?.as_char_stream()?;
    let radix = match &node.args[..] {
        [] => 10,
        [Item::Number(radix)] => radix.try_cast_within(2..=36)?,
        _ => return Err(StreamError::usage(&node.head))
    };
    let st = stm.iter().transposed().map(|ch| -> SResult<char> {
        check_stop!();
        match ch? {
            Char::Single(c) if c.is_ascii() && (c.is_digit(radix) || c == '-' || c == '+') => Ok(c),
            c => Err(StreamError::with_expr("invalid character", &c))
        }
    }).collect::<SResult<String>>()?;
    match Number::from_str_radix(&st, radix) {
        Ok(num) => Ok(Item::new_number(num)),
        Err(_) => Err("invalid input".into())
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
        test_eval!("\"+0\".strnum" => "0");
        test_eval!("\"-FFFFFFFFFFFFFFFFFFFF\".strnum(16)" => "-1208925819614629174706175");
        test_eval!("\"A\".strnum" => err);
        test_eval!("\"A\".strnum(11)" => "10");
        test_eval!("\"B\".strnum(11)" => err);
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("numstr", eval_numstr, r#"
Converts `number` to a string.
If `base` is given, it needs to be between 2 and 36 (inclusive). If omitted, it defaults to 10 (decadic).
If `min_width` is given, the string is zero-padded if shorter.
= number.?
= number.?(base)
= number.?(base, min_width)
> 42.? => "42"
> (-15).?(10, 5) => "-00015" ; sign does not count towards width
> 42.?(2) => "101010"
> 65535.?(16) => "FFFF" ; base-11 to base-36 digits are uppercase
: strnum
: numdig
"#);
    symbols.insert("strnum", eval_strnum, r#"
Converts `string` into a number.
If `base` is given, it needs to be between 2 and 36 (inclusive). If omitted, it defaults to 10 (decadic).
= string.?
= string.?(base)
> "-42".? => -42
> "ffff".?(16) => 65535 ; both uppercase and lowercase accepted
: numstr
: dignum
"#);
}

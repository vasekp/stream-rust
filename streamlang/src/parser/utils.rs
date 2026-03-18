use super::*;

pub fn parse_string(slice: &str) -> Result<String, ParseError<'_>> {
    let mut ret = String::new();
    // First and last characters are guaranteed to be ' or " and thus single-byte
    let inner = &slice[1..(slice.len() - 1)];
    let mut it = inner.char_indices().peekable();
    while let Some((index, c)) = it.next() {
        if c == '\\' {
            match it.next().unwrap().1 { // \ is guaranteed to be followed by at least 1 char
                d @ ('\\' | '\'' | '"') => ret.push(d),
                'n' => ret.push('\n'),
                'r' => ret.push('\r'),
                't' => ret.push('\t'),
                _ => {
                    let end_index = match it.peek() {
                        Some(&(pos, _)) => pos,
                        None => inner.len()
                    };
                    return Err(ParseError::new("invalid escape sequence", &inner[index..end_index]));
                }
            }
        } else {
            ret.push(c);
        }
    }
    Ok(ret)
}

pub fn parse_char(slice: &str) -> Result<Char, ParseError<'_>> {
    let content = parse_string(slice)?;
    if content.is_empty() {
        Err(ParseError::new("empty character", slice))
    } else {
        Ok(Char::from(content.as_str()))
    }
}

pub fn parse_basenum(slice: &str) -> Result<Number, ParseError<'_>> {
    match slice.split('_').collect::<Vec<_>>()[..] {
        [base_str, value_str] if !value_str.is_empty() => {
            let base = base_str.parse::<u32>()
                .map_err(|_| ParseError::new("invalid base", base_str))?;
            if !matches!(base, 2..=36) {
                return Err(ParseError::new("invalid base", base_str));
            }
            Number::from_str_radix(value_str, base)
                .map_err(|_| ParseError::new(format!("invalid digits in base {base}"), value_str))
        },
        _ => Err(ParseError::new("malformed number", slice))
    }
}

pub fn parse_c_basenum(slice: &str) -> Result<Number, ParseError<'_>> {
    Number::from_str_with_radix_prefix(slice)
        .map_err(|_| ParseError::new("invalid digits".to_string(), slice))
}

#[cfg(test)]
#[test]
fn test_basenum() {
    assert!(parse_basenum("2_").is_err()); // malformed number
    assert!(parse_basenum("2_1_").is_err()); // malformed number
    assert!(parse_basenum("2_1_0").is_err()); // malformed number
    assert!(parse_basenum("1_0").is_err()); // invalid base
    assert!(parse_basenum("0_0").is_err()); // invalid base
    assert!(parse_basenum("37_0").is_err()); // invalid base
    assert!(parse_basenum("999999999999999999_0").is_err()); // invalid base
    assert!(parse_basenum("2a_0").is_err()); // invalid base
    assert_eq!(parse_basenum("2_0"), Ok(Number::zero()));
    assert_eq!(parse_basenum("2_101"), Ok(Number::from(5)));
    assert!(parse_basenum("2_102").is_err()); // invalid digits in base 2
    assert_eq!(parse_basenum("10_999999999999999999999999"), Ok("999999999999999999999999".parse().unwrap()));
    assert_eq!(parse_basenum("16_fffFFffFFfFfFFFFffFF"), Ok("1208925819614629174706175".parse().unwrap()));

    assert_eq!(parse_c_basenum("0b111"), Ok(Number::from(7)));
    assert!(parse_c_basenum("0b112").is_err());
    assert!(parse_c_basenum("0b").is_err());
    assert!(parse_c_basenum("0c123").is_err());
    assert_eq!(parse_c_basenum("0x111"), Ok(Number::from(273)));
    assert_eq!(parse_c_basenum("0xFf"), Ok(Number::from(255)));
    assert!(parse_c_basenum("0xFG").is_err());
    assert_eq!(parse_c_basenum("0o123"), Ok(Number::from(83)));
    assert!(parse_c_basenum("0o789").is_err());
}

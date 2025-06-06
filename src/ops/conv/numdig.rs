use crate::base::*;

fn eval_numdig(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    let (num, radix) = match &rnode {
        RNodeS { source: Item::Number(num), args: RArgs::Zero, .. } => (num, 10),
        RNodeS { source: Item::Number(num), args: RArgs::One(Item::Number(radix)), .. } =>
            match radix.to_u32() {
                Some(radix) if (2..=256).contains(&radix) => (num, radix),
                _ => return Err(StreamError::new("radix must be between 2 and 256", rnode))
            },
        _ => return Err(StreamError::new("expected: number.numdig or number.numdig(radix)", rnode))
    };
    if num.is_negative() {
        return Err(StreamError::new("can only accept nonnegative numbers", rnode));
    }
    let (_, vec) = num.to_radix_be(radix);
    let digits = vec.into_iter()
        .map(Item::new_number)
        .collect::<Vec<_>>();
    Ok(Item::from(digits))
}

fn eval_dignum(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_all(env)?.resolve_source()?;
    let (s, radix) = match &rnode {
        RNodeS { source: Item::Stream(s), args: RArgs::Zero, .. } => (s, 10),
        RNodeS { source: Item::Stream(s), args: RArgs::One(Item::Number(radix)), .. } =>
            match radix.to_u32() {
                Some(radix) if (2..=256).contains(&radix) => (s, radix),
                _ => return Err(StreamError::new("radix must be between 2 and 256", rnode))
            },
        _ => return Err(StreamError::new("expected: stream.dignum or stream.dignum(radix)", rnode))
    };
    if s.is_empty() {
        return Err(StreamError::new("stream is empty", rnode));
    }
    let vec = try_with!(rnode, s.iter().map(|item| {
            check_stop!();
            item?.into_num()?
                .to_u8()
                .ok_or(BaseError::from("invalid digit"))
        })
        .collect::<Result<Vec<u8>, _>>()?);
    let num = Number::from_radix_be(num::bigint::Sign::Plus, &vec, radix)
        .ok_or_else(|| StreamError::new("invalid input", rnode))?;
    Ok(Item::Number(num))
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_numdig() {
        use super::*;
        use crate::parser::parse;
        test_eval!("(2^100).numdig" => "[1, 2, 6, 7, 6, ...]");
        test_eval!("(-15).numdig" => err);
        test_eval!("0.numdig" => "[0]");
        test_eval!("(16^20-2).numdig(16)" => "[15, 15, 15, 15, 15, ...]");
        test_eval!("42.numdig(2)" => "[1, 0, 1, 0, 1, ...]");
        test_eval!("42.numdig(36)" => "[1, 6]");
        test_eval!("42.numdig(37)" => "[1, 5]");
        test_eval!("42.numdig(0)" => err);
        test_eval!("42.numdig(1)" => err);
        test_eval!("42.numdig(-1)" => err);
        test_eval!("65535.numdig(255)" => "[1, 2, 0]");
        test_eval!("65535.numdig(256)" => "[255, 255]");
        test_eval!("65532.numdig(257)" => err);
    }

    #[test]
    fn test_dignum() {
        use super::*;
        use crate::parser::parse;
        test_eval!("[1, 6].dignum" => "16");
        test_eval!("[0, 0].dignum" => "0");
        test_eval!("[10].dignum" => err);
        test_eval!("[-1].dignum" => err);
        test_eval!("[].dignum" => err);
        test_eval!("[1, 6].dignum(36)" => "42");
        test_eval!("[1, 6].dignum(255)" => "261");
        test_eval!("[254, 254].dignum(256)" => "65278");
        test_eval!("[1, 6].dignum(257)" => err);
        test_eval!("[256].dignum(255)" => err);
        test_eval!("[256].dignum(256)" => err);
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("numdig", eval_numdig);
    keywords.insert("dignum", eval_dignum);
}

pub(crate) fn op_rules(op: &str) -> std::result::Result<(u32, bool), &'static str> {
    match op {
        "=" => Ok((1, true)),
        "|" => Ok((2, true)),
        "&" => Ok((3, true)),
        "==" => Ok((4, true)),
        "<>" => Ok((4, true)),
        ">" => Ok((4, true)),
        "<" => Ok((4, true)),
        ">=" => Ok((4, true)),
        "<=" => Ok((4, true)),
        ">>" => Ok((4, true)),
        "<<" => Ok((4, true)),
        ">>=" => Ok((4, true)),
        "<<=" => Ok((4, true)),
        "~" => Ok((5, true)),
        ".." => Ok((6, false)),
        "%" => Ok((7, false)),
        "+" => Ok((8, true)),
        "-" => Ok((8, false)),
        "!" => Ok((8, false)),
        "*" => Ok((9, true)),
        "/" => Ok((9, false)),
        "^" => Ok((10, false)),
        _ => Err("undefined operator")
    }
}

pub(crate) fn op_prec(op: &str) -> std::result::Result<u32, &'static str> {
    op_rules(op).map(|(prec, _multi)| prec)
}

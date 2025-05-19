use crate::base::ParseError;

pub(crate) fn op_rules(op: &str) -> Result<(u32, bool), ParseError> {
    match op {
        "=" => Ok((1, true)),
        "|" => Ok((2, true)),
        "&" => Ok((3, true)),
        "!" => Ok((4, true)),
        "==" => Ok((5, true)),
        "<>" => Ok((5, true)),
        ">" => Ok((5, true)),
        "<" => Ok((5, true)),
        ">=" => Ok((5, true)),
        "<=" => Ok((5, true)),
        ">>" => Ok((5, true)),
        "<<" => Ok((5, true)),
        ">>=" => Ok((5, true)),
        "<<=" => Ok((5, true)),
        "~" => Ok((6, true)),
        "+" => Ok((7, true)),
        "-" => Ok((7, false)),
        "*" => Ok((8, true)),
        "/" => Ok((8, false)),
        "^" => Ok((9, false)),
        ".." => Ok((10, false)),
        _ => Err(ParseError::new("undefined operator", op))
    }
}

pub(crate) fn op_prec(op: &str) -> Result<u32, ParseError> {
    op_rules(op).map(|(prec, _multi)| prec)
}

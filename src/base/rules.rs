pub(crate) fn op_rules(op: &str) -> (u32, bool) {
    match op {
        "==" => (1, true),
        "<>" => (1, true),
        ">" => (1, true),
        "<" => (1, true),
        ">=" => (1, true),
        "<=" => (1, true),
        "=" => (2, true),
        "~" => (3, true),
        "+" => (4, true),
        "-" => (4, false),
        "*" => (5, true),
        "/" => (5, false),
        "^" => (6, false),
        ".." => (7, false),
        _ => todo!("operator '{op}' prec")
    }
}

pub(crate) fn op_prec(op: &str) -> u32 {
    op_rules(op).0
}

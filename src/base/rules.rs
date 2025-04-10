pub(crate) fn op_rules(op: &str) -> (u32, bool) {
    match op {
        "~" => (1, true),
        "+" => (2, true),
        "-" => (2, false),
        "*" => (3, true),
        "/" => (3, false),
        "^" => (4, false),
        ".." => (5, false),
        _ => todo!()
    }
}

pub(crate) fn op_prec(op: &str) -> u32 {
    op_rules(op).0
}

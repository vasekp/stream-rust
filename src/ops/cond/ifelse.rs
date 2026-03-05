use crate::base::*;

fn eval_if(node: &Node, env: &Env) -> Result<Item, StreamError> {
    node.check_no_source()?;
    let [cond, true_expr, false_expr] = &node.args[..] else {
        return Err(StreamError::new0("expected: if(cond, expr, expr)"));
    };
    let expr = if cond.eval(env)?.to_bool()? { true_expr } else { false_expr };
    expr.eval(env)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_if() {
        use super::*;

        test_eval!("if(true, \"\", [])" => "\"\"");
        test_eval!("if(false, \"\", [])" => "[]");
        test_eval!("if(0, \"\", [])" => err);
        test_eval!("seq:{if(#<4,-#,#)}" => "[-1, -2, -3, 4, 5, ...]");
        test_eval!("[1,'a',\"a\"]:{if(#.isnum,-#,#+1)}" => "[-1, 'b', \"b\"]");
    }
}

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("if", eval_if, r#"
If `condition` evaluates to `true`, evaluates `a`, otherwise evaluates `b`.
* The other result is not evaluated.
= ?(condition, a, b)
> ?seq:{?(#.?isodd, -#, #)} => [-1, 2, -3, 4, -5, ...]
> ["12", "+3", "xx"]:{?(#.?isnumeric, #.?strnum, #)} => [12, 3, "xx"] ; "xx".strnum would be an error
: select
: while
"#);
}

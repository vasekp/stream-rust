use crate::base::*;

fn eval_if(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.resolve_no_source()?;
    let RNodeNS { args: RArgs::Three(cond, ..), .. } = &rnode else {
        return Err(StreamError::new("expected: if(cond, expr, expr)", rnode));
    };
    let cond_v = match cond.clone().eval(env)? {
        Item::Bool(value) => value,
        item => return Err(StreamError::new(format!("expected bool, found {:?}", item), rnode))
    };
    let RArgs::Three(_, true_expr, false_expr) = rnode.args else { unreachable!() };
    let expr = if cond_v { true_expr } else { false_expr };
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

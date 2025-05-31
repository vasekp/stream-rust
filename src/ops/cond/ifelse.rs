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
        //use super::*;
        use crate::parser::parse;
        assert_eq!(parse("if(true, \"\", [])").unwrap().eval_default().unwrap().to_string(), "\"\"");
        assert_eq!(parse("if(false, \"\", [])").unwrap().eval_default().unwrap().to_string(), "[]");
        assert!(parse("if(0, \"\", [])").unwrap().eval_default().is_err());
        assert_eq!(parse("seq:{if(#<4,-#,#)}").unwrap().eval_default().unwrap().to_string(), "[-1, -2, -3, 4, 5, ...]");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("if", eval_if);
}

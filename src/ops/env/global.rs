use crate::base::*;

fn eval_global(mut node: Node, env: &Env) -> Result<Item, StreamError> {
    if node.args.len() != 1 {
        return Err(StreamError::new("exactly 1 argument required", node));
    }
    let body = match (node.source, node.args.pop().unwrap()) { // just checked len = 1
        (None, Expr::Imm(item)) => return Ok(item),
        (Some(_), Expr::Imm(item)) => return Err(StreamError::new("no source accepted", item)),
        (None, Expr::Eval(body)) => body,
        (Some(source), Expr::Eval(body)) => body.with_source(*source)?,
        (_, expr @ Expr::Repl(_)) => return Err(StreamError::new("out of context", expr))
    };
    let body = body.eval_all(env)?;
    Node::from(body).eval(&Default::default())
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("global", eval_global);
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_with() {
        use crate::parser::parse;
        assert_eq!(parse("with(len=1, global(range(3).len))").unwrap().eval_default().unwrap().to_string(), "3");
        assert_eq!(parse("with(a=1, a.global{#+#1}(a))").unwrap().eval_default().unwrap().to_string(), "2");
        assert_eq!(parse("with(a={#+#1}, 1.a(2))").unwrap().eval_default().unwrap().to_string(), "3");
        assert!(parse("with(a={#+#1}, global(1.a(2)))").unwrap().eval_default().is_err());
        assert!(parse("with(a={#+#1}, 1.global{a}(2))").unwrap().eval_default().is_err());
        assert_eq!(parse("1.global{2}").unwrap().eval_default().unwrap().to_string(), "2");
        assert!(parse("1.global(2)").unwrap().eval_default().is_err());
        assert!(parse("with(a=1, global{a})").unwrap().eval_default().is_err());
        assert_eq!(parse("with(a=1, global{with(a=2, a)})").unwrap().eval_default().unwrap().to_string(), "2");
        assert_eq!(parse("with(a=1, global{with(a=#1+1, a)}(a))").unwrap().eval_default().unwrap().to_string(), "2");
    }
}

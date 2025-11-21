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

pub fn init(symbols: &mut crate::symbols::Symbols) {
    symbols.insert("global", eval_global);
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_with() {
        use super::*;

        test_eval!("with(len=1, global(range(3).len))" => "3");
        test_eval!("with(a=1, a.global{#+#1}(a))" => "2");
        test_eval!("with(a={#+#1}, 1.a(2))" => "3");
        test_eval!("with(a={#+#1}, global(1.a(2)))" => err);
        test_eval!("with(a={#+#1}, 1.global{a}(2))" => err);
        test_eval!("1.global{2}" => "2");
        test_eval!("1.global(2)" => err);
        test_eval!("with(a=1, global{a})" => err);
        test_eval!("with(a=1, global{with(a=2, a)})" => "2");
        test_eval!("with(a=1, global{with(a=#1+1, a)}(a))" => "2");
    }
}

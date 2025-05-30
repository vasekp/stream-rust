use crate::base::*;

fn eval_count(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_source(env)?;
    let (stm, body) = match &rnode {
        RNodeS { source: Item::Stream(stm), args: RArgs::One(Expr::Eval(body)), .. }
            if body.source.is_none() => (stm, body),
        _ => return Err(StreamError::new("expected: stream.count{cond}", rnode))
    };
    let mut count = 0;
    try_with!(rnode, {
        for item in stm.iter() {
            check_stop!();
            match body.clone().with_source(item?.into())?.eval(env)? {
                Item::Bool(cond) => if cond { count.inc() },
                other => return Err(format!("expected bool, found {:?}", other).into())
            }
        }
        Ok(Item::new_number(count))
    })
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_count() {
        //use super::*;
        use crate::parser::parse;
        assert_eq!(parse("range(5).count{true}").unwrap().eval_default().unwrap().to_string(), "5");
        assert_eq!(parse("range(5).count{false}").unwrap().eval_default().unwrap().to_string(), "0");
        assert_eq!(parse("range(-5,5).count{#<0}").unwrap().eval_default().unwrap().to_string(), "5");
        assert!(parse("range(5).count{#}").unwrap().eval_default().is_err());
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("count", eval_count);
}

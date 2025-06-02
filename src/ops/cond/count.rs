use crate::base::*;

fn eval_count(node: Node, env: &Env) -> Result<Item, StreamError> {
    let rnode = node.eval_source(env)?;
    let (stm, cond) = match &rnode {
        RNodeS { source: Item::Stream(stm), args: RArgs::One(Expr::Eval(cond)), .. } => (stm, cond),
        _ => return Err(StreamError::new("expected: stream.count{cond}", rnode))
    };
    let mut count = 0;
    try_with!(rnode, {
        for item in stm.iter() {
            check_stop!();
            match cond.clone().with_source(item?.into())?.eval(env)? {
                Item::Bool(value) => if value { count.inc() },
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
        use super::*;
        use crate::parser::parse;

        test_eval!("range(5).count{true}" => "5");
        test_eval!("range(5).count{false}" => "0");
        test_eval!("range(-5,5).count{#<0}" => "5");
        test_eval!("range(5).count{#}" => err);
        test_eval!("range(5).count([].len)" => err);
        test_eval!("[].count{1}" => "0");
        test_eval!("[].count(1)" => err);
        test_eval!("[].count([].len)" => "0");
    }
}

pub fn init(keywords: &mut crate::keywords::Keywords) {
    keywords.insert("count", eval_count);
}
